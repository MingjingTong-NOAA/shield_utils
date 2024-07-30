!-------------------------------------------------------------------------------
!> @brief control module of FV3_DA
!> @author Xi.Chen <xi.chen@noaa.gov>
!> @date 02/09/2016
!
!  REVISION HISTORY:
!  02/09/2016 - Initial Version
!-------------------------------------------------------------------------------

module fv3_da_ctrl_mod

  use io_nc_util_mod, only: check, &
                            io_nc_create_file, &
                            io_nc_close_file, &
                            io_nc_add_dim
  use netcdf

  implicit none

  private

  real, parameter :: pi = 3.141592653589793

  public :: get_parameters
  public :: read_gaus_dimensions
  public :: read_gaus_grid
  public :: netcdf_err, nc_get_cube_variable, nc_put_variable

  ! -----------------------------------------------------------------------
  ! namelist parameters
  ! -----------------------------------------------------------------------

  integer :: ntiles=6
  integer :: finer_steps=0
  integer :: nvar3dout=16
  integer :: yy=2020
  integer :: mm=1
  integer :: dd=1
  integer :: hh=0
  integer :: ideflate=1
  integer :: nbits=14
  integer :: quantize_nsd=0
  integer :: atmos_nthreads=1
  real    :: fhr=0.
  logical :: read_res=.false.
  logical :: write_res=.false.
  logical :: memphis=.false.
  logical :: write_nemsio=.false.
  logical :: write_nemsioflip=.true.
  logical :: fill_missing=.false.
  logical :: rmhydro=.false.
  logical :: pseudo_ps=.false.
  logical :: atminc=.false.
  logical :: get_weights_only=.false.
  logical :: time_unlimited=.false.
  character(len=120) :: grid_file="grid_spec"
  character(len=120) :: gaus_file="gaus_N48"
  character(len=120) :: data_out="atmf"
  character(len=120) :: uname="ua"
  character(len=120) :: vname="va"
  character(len=120) :: missing_value="missing_value"
  character(len=120) :: data_file(3)
  character(len=120) :: quantize_mode="quantize_bitround"

  ! -----------------------------------------------------------------------
  ! namelist
  ! -----------------------------------------------------------------------

  namelist /fv3_da_nml/ ntiles, finer_steps, nvar3dout, atminc,     &
                        write_res, read_res, memphis, write_nemsio, &
                        grid_file, data_file, data_out,             &
                        uname, vname, missing_value, time_unlimited,&
                        fill_missing, write_nemsioflip,rmhydro,     &
                        pseudo_ps, get_weights_only, gaus_file,     &
                        atmos_nthreads, yy,mm, dd, hh, fhr,         &
                        ideflate, nbits, quantize_nsd, quantize_mode

  public &
      ntiles, finer_steps, nvar3dout, yy, mm, dd, hh, ideflate, nbits, &
      quantize_nsd, quantize_mode,time_unlimited,                      &
      fhr, read_res, write_res, memphis, fill_missing, write_nemsio,   &
      write_nemsioflip, rmhydro, pseudo_ps, atminc, get_weights_only,  &
      grid_file, gaus_file, uname, vname, missing_value, data_file, data_out

contains
  !=============================================================================
  !> @brief output the parameters for c2l process
  subroutine get_parameters
    !------------------------------------------------------------------!
    ! local variables                                                  !
    !------------------------------------------------------------------!
    integer :: ios, i, nlon_fine, nlat_fine
    character(len=80) :: filename
    logical :: exists
    integer :: base_cpu
    integer :: get_cpu_affinity, omp_get_thread_num 

    !------------------------------------------------------------------!
    ! read file names with cubed sphere data from namelist file        !
    !------------------------------------------------------------------!
    filename = "fv3_da.nml"
    inquire(file=filename,exist=exists)

    if (.not. exists) then
       write(6,100) trim(filename)
100    format (/,"namelist file ",a," doesn't exist",/)
       stop
    else
       data_file(1)="fv_core.res"
       data_file(2)="fv_tracer.res"
       data_file(3)="phy_data"
       !---------------------------------------------------------------!
       ! read main namelist                                            !
       !---------------------------------------------------------------!
       open (10,file=filename)
       read (10,nml=fv3_da_nml,iostat=ios)
       close(10)
       if (ios > 0) then
          write(6,101) trim(filename), ios
101       format(/,"fv3_da_nml ERROR: reading ",a,", iostat=",i4,/)
          stop
       endif

       if (ntiles/=6) write(6,112) ntiles
112    format(/,"WARNING: ntiles not equal 6! ntiles = ",i3)

       if (atminc) then
         rmhydro=.false.
         pseudo_ps=.false.
       endif

    endif

!$      base_cpu = get_cpu_affinity()
!$      call omp_set_num_threads(atmos_nthreads)
!$OMP PARALLEL NUM_THREADS(atmos_nthreads)
!$         call set_cpu_affinity(base_cpu + omp_get_thread_num())
!$OMP END PARALLEL

  end subroutine get_parameters
  !=============================================================================
  !> @brief get nlon and nlat from gaus_file
  subroutine read_gaus_dimensions(nlon, nlat)
    integer, intent(out) :: nlon, nlat
    !------------------------------------------------------------------!
    ! local variables                                                  !
    !------------------------------------------------------------------!
    integer :: ncid, lon_id, lat_id

    character(len=120) :: filename
    logical :: exists

    ! form file name and check existance
    write(filename,100) trim(gaus_file)
100 format(a,'.nc')
    inquire(file=filename, exist=exists)
    if (.not. exists) then
       print 110, trim(filename)
110    format(/,"Gaussian grid file ",a," doesn't exist",/)
       stop
    endif
    ! open nc file
    call check(nf90_open(trim(filename), 0, ncid))

    ! get nlon
    call check(nf90_inq_dimid(ncid, "lon", lon_id))
    call check(nf90_inquire_dimension(ncid, lon_id, len=nlon))

    ! get nlat
    call check(nf90_inq_dimid(ncid, "lat", lat_id))
    call check(nf90_inquire_dimension(ncid, lat_id, len=nlat))

    write(6,106) nlon, nlat
106 format(" will output data on a Gaussian grid with nlon =",i4,", nlat =",i4)

    ! close nc file
    call check(nf90_close(ncid))

  end subroutine read_gaus_dimensions
  !=============================================================================
  !> @brief get lon and lat from gaus_file
  subroutine read_gaus_grid(xlon, ylat, nlon, nlat)
    !------------------------------------------------------------------!
    ! initialize Gaussian grid                                         !
    !------------------------------------------------------------------!
    integer, intent(in)  :: nlon, nlat
    real,    intent(out) :: xlon(nlon), ylat(nlat)
    !------------------------------------------------------------------!
    ! local variables                                                  !
    !------------------------------------------------------------------!
    integer :: ncid, lon_id, lat_id, ndims, dimids(1)

    character(len=120) :: filename
    logical :: exists

    ! form file name and check existance
    write(filename,100) trim(gaus_file)
100 format(a,'.nc')
    inquire(file=filename, exist=exists)
    if (.not. exists) then
       print 110, trim(filename)
110    format(/,"Gaussian grid file ",a," doesn't exist",/)
       stop
    endif

    ! open nc file
    call check(nf90_open(trim(filename), 0, ncid))

    ! get nlon
    call check(nf90_inq_varid(ncid, "lon", lon_id))
    call check(nf90_get_var(ncid, lon_id, xlon))

    xlon = xlon*pi/180.

    ! get nlat
    call check(nf90_inq_varid(ncid, "lat", lat_id))
    call check(nf90_get_var(ncid, lat_id, ylat))

    ylat = ylat*pi/180.

    ! close nc file
    call check(nf90_close(ncid))

  end subroutine read_gaus_grid
  !=============================================================================

  subroutine netcdf_err(err, string)

  use netcdf

  implicit none

  character(len=*), intent(in) :: string
  integer, intent(in)          :: err
 
  character(len=256)           :: errmsg
 
  if( err.eq.nf90_noerr )return
 
  errmsg = nf90_strerror(err)
  print*,''
  print*,'** FATAL ERROR: ', trim(string), ': ', trim(errmsg)
  print*,'STOP.'
  call errexit(22)
 
  return
  end subroutine netcdf_err

  !=============================================================================

  subroutine nc_get_cube_variable(ncid, varid, vartype, varoffset, varscale, &
                                  varndims, npx, npy, nlev, ntiles, start, count, &
                                  var_cubsph)

    integer, intent(in) :: varid, vartype, varndims, npx, npy, nlev, ntiles
    integer, intent(in) :: ncid(ntiles)
    integer, intent(in) :: start(4), count(4)
    real, intent(in) :: varoffset, varscale
    real*4, dimension(:,:,:), allocatable :: var_r4
    real*8, dimension(:,:,:), allocatable :: var_r8
    integer*2, dimension(:,:,:), allocatable :: var_i2
    real, intent(out) :: var_cubsph(0:npx,0:npy,nlev,ntiles)
    integer :: itile,status

    if (varndims == 3 .or. varndims == 4) then
       if (vartype==nf90_double) then
          if (varndims == 3) allocate(var_r8(npx-1,npy-1,1))
          if (varndims == 4) allocate(var_r8(npx-1,npy-1,nlev))
          do itile=1,ntiles
             status = nf90_get_var(ncid(itile), varid, var_r8, start=start, count=count)
             if (varndims == 3) &
                var_cubsph(1:npx-1,1:npy-1,1,itile)=var_r8(:,:,1)
             if (varndims == 4) &
                var_cubsph(1:npx-1,1:npy-1,1:nlev,itile)=var_r8(:,:,:)
          enddo
          deallocate(var_r8)
       elseif (vartype==nf90_float) then
          if (varndims == 3) allocate(var_r4(npx-1,npy-1,1))
          if (varndims == 4) allocate(var_r4(npx-1,npy-1,nlev))
          do itile=1,ntiles
             status = nf90_get_var(ncid(itile), varid, var_r4, start=start, count=count)
             if (varndims == 3) &
                var_cubsph(1:npx-1,1:npy-1,1,itile)=var_r4(:,:,1)
             if (varndims == 4) &
                var_cubsph(1:npx-1,1:npy-1,1:nlev,itile)=var_r4(:,:,:)
          enddo
          deallocate(var_r4)
       elseif (vartype==nf90_short) then
          if (varndims == 3) allocate(var_i2(npx-1,npy-1,1))
          if (varndims == 4) allocate(var_i2(npx-1,npy-1,nlev))
          do itile=1,ntiles
             status = nf90_get_var(ncid(itile), varid, var_i2, start=start, count=count)
             if (varndims == 3) &
                var_cubsph(1:npx-1,1:npy-1,1,itile)=varscale*var_i2(:,:,1)+varoffset
             if (varndims == 4) &
                var_cubsph(1:npx-1,1:npy-1,1:nlev,itile)=varscale*var_i2(:,:,:)+varoffset
          enddo
          deallocate(var_i2)
       else
          print*," vartype neither nf90_double nor nf90_float of nf90_short: ",vartype
          stop
       endif
    else
       print*," unexpected number of dimensions for varid ", varid
       stop
    endif

  end subroutine nc_get_cube_variable

  subroutine nc_put_variable(ncid, varid, vartype, varoffset, varscale, &
                             nlon, nlat, nlev, ncflip, kflip, var_latlon, &
                             ideflate, nbits, compress_err, misval)

    integer, intent(in) :: ncid, varid, vartype, nlon, nlat, nlev
    integer, intent(in) :: ideflate, nbits
    real, intent(in) :: varoffset, varscale, misval
    real, intent(in) :: var_latlon(nlon,nlat,nlev)
    logical, intent(in) :: ncflip, kflip
    integer, dimension(:), allocatable   :: start_idx
    real*4, dimension(:,:,:), allocatable :: var_r4, var_r4_save
    real*8, dimension(:,:,:), allocatable :: var_r8
    integer*2, dimension(:,:,:), allocatable :: var_i2
    real, dimension(:,:,:), allocatable :: var_tmp, var_new
    real*4 :: dataMin, dataMax, compress_err
    integer :: status,j,k,start_i, start_j

    start_i = 1
    start_j = 1
    
    if (nlev == 1) then
       start_idx = [start_i,start_j,        1]
    else
       allocate(start_idx(4))
       start_idx = [start_i,start_j,1,        1]
    endif

    if (misval /= 0.0) then
       status = nf90_put_att(ncid, varid, "missing_value", misval)
    endif

    allocate(var_tmp(nlon,nlat,nlev))
    if ( ncflip ) then
       do j=1,nlat
          var_tmp(:,j,:)=var_latlon(:,nlat-j+1,:)
       end do
    else
       var_tmp=var_latlon
    end if

    allocate(var_new(nlon,nlat,nlev))
    if ( kflip ) then
       do k=1,nlev
          var_new(:,:,k)=var_tmp(:,:,nlev-k+1)
       end do 
    else
       var_new=var_tmp
    end if 

    if (vartype==nf90_double) then
       allocate(var_r8(nlon,nlat,nlev))
       var_r8(:,:,:)=var_new(:,:,:)
       status = nf90_put_var(ncid, varid, var_r8, start=start_idx)
       deallocate(var_r8)
    elseif (vartype==nf90_float) then
       allocate(var_r4(nlon,nlat,nlev))
       if (ideflate > 0 .and. nbits > 0 .and. nlev > 1) then
          allocate(var_r4_save(nlon,nlat,nlev))
          var_r4_save = var_new
          dataMax = maxval(var_new); dataMin = minval(var_new)
          var_r4 = quantized(var_r4_save, nbits, dataMin, dataMax)
          ! compute max abs compression error, save as a variable
          ! attribute.
          compress_err = maxval(abs(var_r4_save-var_r4))
          deallocate(var_r4_save)
       else
          var_r4 = var_new
       endif
       status = nf90_put_var(ncid, varid, var_r4, start=start_idx)
       deallocate(var_r4)
    elseif (vartype==nf90_short) then
       allocate(var_i2(nlon,nlat,nlev))
       var_i2(:,:,:)=(var_new(:,:,:)-varoffset)/varscale
       status = nf90_put_var(ncid, varid, var_i2, start=start_idx)
       deallocate(var_i2)
    endif
    deallocate(var_tmp,var_new)
    deallocate(start_idx)

  end subroutine nc_put_variable

  elemental real function quantized(dataIn, nbits, dataMin, dataMax)
    integer, intent(in) :: nbits
    real(4), intent(in) :: dataIn, dataMin, dataMax
    real(4) offset, scale_fact
    ! convert data to 32 bit integers in range 0 to 2**nbits-1, then cast
    ! cast back to 32 bit floats (data is then quantized in steps
    ! proportional to 2**nbits so last 32-nbits in floating
    ! point representation should be zero for efficient zlib compression).
    scale_fact = (dataMax - dataMin) / (2**nbits-1); offset = dataMin
    quantized = scale_fact*(nint((dataIn - offset) / scale_fact)) + offset
  end function quantized

end module fv3_da_ctrl_mod


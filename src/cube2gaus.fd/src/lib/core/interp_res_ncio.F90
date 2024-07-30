!-------------------------------------------------------------------------------
!> @brief interpolate the restart files
!>        adapted from MH's interpolate_data subroutine
!> @author Xi.Chen <xi.chen@noaa.gov>
!> @date 02/08/2016
!
!  REVISION HISTORY:
!  02/08/2016 - Initial Version
!-------------------------------------------------------------------------------

module interp_res_ncio_mod

  use cub2latlon_mod, only: init_latlon_grid, &
                            do_c2l_interpolation, &
                            do_latlon_coarsening
  use fv_timing_mod, only: timing_on, timing_off, timing_prt
  use fv3_da_out_api_mod, only: fv3_da_out_type, &
                                fv3_da_out_interp_scalar, &
                                fv3_da_out_interp_uv
  use netcdf
  use nemsio_module
  use fv3_da_ctrl_mod, only: netcdf_err, nc_get_cube_variable, nc_put_variable
  use fv3_da_ctrl_mod, only: finer_steps, nvar3dout,                   &
                        read_res, write_res, memphis, write_nemsio,    &
                        grid_file, data_file, data_out,                &
                        uname, vname, missing_value,time_unlimited,    &
                        fill_missing,write_nemsioflip,ideflate, nbits, &
                        quantize_nsd,quantize_mode,                    &
                        rmhydro, pseudo_ps, get_weights_only,atminc,   &
                        gaus_file, yy, mm, dd, hh, fhr

  use io_nc_util_mod, only: check

  implicit none
  private
  integer :: im, jm, nrec, ncld, nvar3d, nvar2d
  integer :: idate(6)
  real :: ptop
  logical :: exists

  character(16),dimension(:),allocatable  :: recname,reclevtyp

  public :: interp_res_ncio

  integer :: npz
  real, parameter :: pi = 3.141592653589793
  real, parameter :: grav = 9.80665
  real, parameter :: grav2=9.80
  real, parameter :: rdgas=287.04
  real, parameter :: stndrd_atmos_ps = 101325.
  real, parameter :: stndrd_atmos_lapse = 0.0065
!  real, parameter :: RDGAS  = 287.05
!  real, parameter :: CP_AIR = 1004.6
!  real, parameter :: kappa  = RDGAS/CP_AIR    !< RDGAS / CP_AIR [dimensionless]
  real, parameter :: kappa = 0.2857356

contains

!----------------------------------------------------------------------------------------
  subroutine ncio_first_call(ncid_out,im,jm,lon,lat,recid,time_varid,ncflip)

    implicit none

    integer, intent(in)                 :: ncid_out,im,jm
    real, intent(in)                    :: lon(im), lat(jm)
 
    integer, intent(out)                :: recid(30)
    integer, intent(out)                :: time_varid
    logical, intent(out)                :: ncflip
!
!** local vars
    integer i, j, k, lm, lunit, rc, error
    integer, parameter :: nrec_all=18
    integer :: ncid, id_dim, id_var, vartype
    integer :: ntrac,nsoil,ntot3d,ncnst
    integer, parameter :: nvar3d_full=16 

    real(4), allocatable   :: dummy_r4(:)
    real(8), allocatable   :: dummy_r8(:)
    real, dimension(:), allocatable   :: ak, bk, pf, ph
    real(4), dimension(:), allocatable   :: pfull, phalf
    real, dimension(:), allocatable  :: lat1d
    real(8), dimension(:,:), allocatable :: array_r8
    integer, dimension(:), allocatable   :: start_idx

    real, parameter :: p_ref = 1.E5
    character(len=nf90_max_name) :: time_units

    character(16) :: recname_all(nrec_all)
    character(120) :: reclongname(nrec_all), recunit(nrec_all)

    integer :: im_dimid, jm_dimid, pfull_dimid, phalf_dimid, time_dimid
    integer :: im_varid, jm_varid, lm_varid, lon_varid, lat_varid
    integer :: pfull_varid, phalf_varid
    integer :: ichunk2d,jchunk2d,ichunk3d,jchunk3d,kchunk3d
    integer :: ishuffle,quant_mode
    integer :: istart, jstart, iend, jend
    logical :: shuffle
    integer, dimension(:), allocatable :: chunksizes

    data recname_all /'ugrd', 'vgrd', 'tmp', 'delz',      &
                      'dpres', 'spfh',  'clwmr', 'rwmr',  &
                      'icmr',  'snmr',  'grle', 'o3mr',   &
                      'dzdt', 'cld_amt', 'cnvw', 'cnvc',  &
                      'pressfc', 'hgtsfc'/

    data reclongname  /'zonal wind', 'meridional wind', 'temperature', & 
                       'height thickness', 'pressure thickness', &
                       'specific humidity', 'cloud water mixing ratio', &
                       'rain mixing ratio', 'cloud ice mixing ratio', &
                       'snow mixing ratio', 'graupel mixing ratio',  &
                       'ozone mixing ratio', 'vertical wind', &
                       'cloud fraction', 'convective cloud water', &
                       'convective cloud fraction', 'surface pressure', &
                       'surface geopotential height'/

    data recunit /'m/sec', 'm/sec', 'K', 'm', 'pa', 'kg/kg', &
                  'kg/kg', 'kg/kg', 'kg/kg', 'kg/kg', 'kg/kg', 'kg/kg', &
                  'm/sec', 'None', 'kg/kg', 'None', 'pa', 'gpm'/

    idate = 0
    idate(1) = yy
    idate(2) = mm
    idate(3) = dd
    idate(4) = hh
    time_units = get_time_units_from_idate(idate)

!** open fv_core.res.nc

    call check(nf90_open("./fv_core.res.nc",nf90_nowrite,ncid))

    call check(nf90_inq_dimid(ncid, 'xaxis_1', id_dim))
    call check(nf90_inquire_dimension(ncid,id_dim,len=lm))
    npz=lm-1

    call check(nf90_def_dim(ncid_out, "grid_xt", im, im_dimid))
    call check(nf90_def_var(ncid_out, "grid_xt", NF90_DOUBLE, im_dimid, im_varid))
    call check(nf90_put_att(ncid_out, im_varid, "long_name", "T-cell longitude"))
    call check(nf90_put_att(ncid_out, im_varid, "cartesian_axis", "X"))
    call check(nf90_put_att(ncid_out, im_varid, "units", "degrees_E"))

    call check(nf90_def_dim(ncid_out, "grid_yt", jm, jm_dimid))
    call check(nf90_def_var(ncid_out, "grid_yt", NF90_DOUBLE, jm_dimid, jm_varid))
    call check(nf90_put_att(ncid_out, jm_varid, "cartesian_axis", "Y"))
    call check(nf90_put_att(ncid_out, jm_varid, "long_name", "T-cell latitude"))
    call check(nf90_put_att(ncid_out, jm_varid, "units", "degrees_N"))

    call check(nf90_def_var(ncid_out, "lon", NF90_DOUBLE, [im_dimid,jm_dimid           ], lon_varid))
    call check(nf90_put_att(ncid_out, lon_varid, "long_name", "T-cell longitude"))
    call check(nf90_put_att(ncid_out, lon_varid, "units", "degrees_E"))
    
    call check(nf90_def_var(ncid_out, "lat", NF90_DOUBLE, [im_dimid,jm_dimid           ], lat_varid)) 
    call check(nf90_put_att(ncid_out, lat_varid, "long_name", "T-cell latitude"))
    call check(nf90_put_att(ncid_out, lat_varid, "units", "degrees_N"))

    call check(nf90_def_dim(ncid_out, "pfull", lm-1, pfull_dimid))
    call check(nf90_def_var(ncid_out, "pfull", NF90_FLOAT, pfull_dimid, varid=pfull_varid))
    call check(nf90_put_att(ncid_out, pfull_varid, "cartesian_axis", "Z"))
    call check(nf90_put_att(ncid_out, pfull_varid, "edges", "phalf"))
    call check(nf90_put_att(ncid_out, pfull_varid, "long_name", "ref full pressure level"))
    call check(nf90_put_att(ncid_out, pfull_varid, "positive", "down"))
    call check(nf90_put_att(ncid_out, pfull_varid, "units", "mb"))

    call check(nf90_def_dim(ncid_out, "phalf", lm, phalf_dimid))
    call check(nf90_def_var(ncid_out, "phalf", NF90_FLOAT, phalf_dimid, varid=phalf_varid))
    call check(nf90_put_att(ncid_out, phalf_varid, "cartesian_axis", "Z"))
    call check(nf90_put_att(ncid_out, phalf_varid, "long_name", "ref half pressure level"))
    call check(nf90_put_att(ncid_out, phalf_varid, "positive", "down"))
    call check(nf90_put_att(ncid_out, phalf_varid, "units", "mb"))

    if (time_unlimited) then
       call check(nf90_def_dim(ncid_out, "time", NF90_UNLIMITED, time_dimid))
    else
       call check(nf90_def_dim(ncid_out, "time", 1, time_dimid))
    end if
    call check(nf90_def_var(ncid_out, "time", NF90_DOUBLE, time_dimid, varid=time_varid))
    call check(nf90_put_att(ncid_out, time_varid, "calendar", "GREGORIAN"))
    call check(nf90_put_att(ncid_out, time_varid, "calendar_type", "GREGORIAN"))
    call check(nf90_put_att(ncid_out, time_varid, "cartesian_axis", "T"))
    call check(nf90_put_att(ncid_out, time_varid, "long_name", "time"))
    call check(nf90_put_att(ncid_out, time_varid, "units", time_units))

    ncnst=10
    ntrac=9
    ncld=5
    nsoil=4
    ntot3d=7
    nvar2d=2
    if (atminc) then
       nvar3d=12
       nvar2d=1
    else if (ntot3d == 7) then
       nvar3d=16
    else
       nvar3d=15
    end if
    if (nvar3dout <= nvar3d) nvar3d=nvar3dout

    recid=0

    ichunk2d=im; jchunk2d=jm
    ichunk3d=im; jchunk3d=jm; kchunk3d=npz
    print *,'ichunk3d, jchunk3d, kchunk3d=', ichunk3d, jchunk3d, kchunk3d
    print *,'ichunk2d, jchunk2d=', ichunk2d, jchunk2d
    print *,'ideflate= ', ideflate, 'quantize_nsd= ', quantize_nsd 
    print *,'nvar3d = ', nvar3d
    print *,'im_dimid,jm_dimid,pfull_dimid,time_dimid=', im_dimid,jm_dimid,pfull_dimid,time_dimid

    ishuffle = NF90_NOSHUFFLE
    ! shuffle filter on when using lossy compression
    if ( quantize_nsd > 0) then
       ishuffle = NF90_SHUFFLE
    end if

    allocate(chunksizes(4)) 
    do j = 1, nvar3d
       call check(nf90_def_var(ncid_out, trim(recname_all(j)), NF90_FLOAT, &
                  [im_dimid,jm_dimid,pfull_dimid,time_dimid], recid(j)))
       print *, 'recname ', j, trim(recname_all(j)), recid(j)
       if (ideflate > 0) then
          chunksizes = [ichunk3d,jchunk3d,kchunk3d,1]
          call check(nf90_def_var_chunking(ncid_out, recid(j), NF90_CHUNKED, chunksizes))
          call check(nf90_def_var_deflate(ncid_out, recid(j), ishuffle, 1, ideflate))
          if (quantize_nsd > 0) then
              if (trim(quantize_mode) == 'quantize_bitgroom') then
                quant_mode = 1
              else if (trim(quantize_mode) == 'quantize_granularbr') then
                quant_mode = 2
              else if (trim(quantize_mode) == 'quantize_bitround') then
                quant_mode = 3
              else
                print *, 'Unknown quantize_mode ', trim(quantize_mode)
                stop "Stopped"
              endif
              call check(nf90_def_var_quantize(ncid_out, recid(j), quant_mode, quantize_nsd))
          end if
       end if
       call nc_put_attribute(ncid_out, recid(j), reclongname(j), recunit(j))
    end do
    deallocate(chunksizes)

    allocate(chunksizes(3))
    if (nvar2d > 0) then
       do j = 1, nvar2d
          call check(nf90_def_var(ncid_out, trim(recname_all(nvar3d_full+j)), NF90_FLOAT, &
                     [im_dimid,jm_dimid,time_dimid], recid(nvar3d+j)))
          print *, 'recname = ', nvar3d_full+j, trim(recname_all(nvar3d_full+j)), recid(nvar3d+j)
          if (ideflate > 0) then
             chunksizes = [ichunk2d,jchunk2d,1]
             call check(nf90_def_var_chunking(ncid_out, recid(nvar3d+j), NF90_CHUNKED, chunksizes))
             call check(nf90_def_var_deflate(ncid_out, recid(nvar3d+j), ishuffle, 1, ideflate))
          endif
          call nc_put_attribute(ncid_out, recid(nvar3d+j), &
                                reclongname(nvar3d_full+j), recunit(nvar3d_full+j))
       end do
    endif
    deallocate(chunksizes)

    allocate(ak(lm),bk(lm))

    error = nf90_inq_varid (ncid, 'ak', id_var)
    call netcdf_err(error, 'READING ak ID' )
    error = nf90_inquire_variable(ncid, id_var, xtype=vartype)
    call netcdf_err(error, 'GETING variable type')
    if (vartype == NF90_DOUBLE) then
       allocate(dummy_r8(lm))
       error=nf90_get_var(ncid, id_var, dummy_r8)
       call netcdf_err(error, 'READING ak' )
       ak=dummy_r8
       call check(nf90_put_att(ncid_out, NF90_GLOBAL, 'ak', dummy_r8))
       error = nf90_inq_varid (ncid, 'bk', id_var)
       call netcdf_err(error, 'READING bk ID' )
       error=nf90_get_var(ncid, id_var, dummy_r8)
       call netcdf_err(error, 'READING bk' )
       bk=dummy_r8
       call check(nf90_put_att(ncid_out, NF90_GLOBAL, 'bk', dummy_r8))
       deallocate(dummy_r8)
    else
       allocate(dummy_r4(lm))
       error=nf90_get_var(ncid, id_var, dummy_r4)
       call netcdf_err(error, 'READING ak' )
       ak=dummy_r4
       call check(nf90_put_att(ncid_out, NF90_GLOBAL, 'ak', dummy_r4))
       error = nf90_inq_varid (ncid, 'bk', id_var)
       call netcdf_err(error, 'READING bk ID' )
       error=nf90_get_var(ncid, id_var, dummy_r4)
       call netcdf_err(error, 'READING bk' )
       bk=dummy_r4
       call check(nf90_put_att(ncid_out, NF90_GLOBAL, 'bk', dummy_r4))
       deallocate(dummy_r4)
    end if

    ptop = ak(1)
    print *,'ptop=', ptop

    call check(nf90_put_att(ncid_out, NF90_GLOBAL, 'grid', 'gaussian'))
    call check(nf90_put_att(ncid_out, NF90_GLOBAL, 'hydrostatic', 'non-hydrostatic'))
    call check(nf90_put_att(ncid_out, NF90_GLOBAL, 'im', im))
    call check(nf90_put_att(ncid_out, NF90_GLOBAL, 'jm', jm))
    call check(nf90_put_att(ncid_out, NF90_GLOBAL, 'ncnsto', ncnst))
    call check(nf90_put_att(ncid_out, NF90_GLOBAL, 'source', 'SHiELD'))

    call check(nf90_enddef(ncid=ncid_out))

    ncflip = .false.
    if (lat(1) < 0.0) ncflip = .true.
    if (ncflip) then
       allocate(lat1d(jm))
       do j=1,jm
          lat1d(j) = lat(jm-j+1)
       end do
    else
       lat1d=lat
    end if

    allocate(array_r8(im,jm))

    istart = lbound(array_r8,1); iend   = ubound(array_r8,1)
    jstart = lbound(array_r8,2); jend   = ubound(array_r8,2)

    allocate(start_idx(2))
    start_idx = [1, 1]

    do j=1,jm
       array_r8(:,j)=lon(:)
    end do
    call check(nf90_put_var(ncid_out, lon_varid, values=array_r8, start=start_idx))
    call check(nf90_put_var(ncid_out, im_varid, values=array_r8(:,jstart), start=[istart], count=[iend-istart+1]))

    do i=1,im
       array_r8(i,:)=lat1d(:)
    end do

    call check(nf90_put_var(ncid_out, lat_varid, values=array_r8, start=start_idx)) 
    call check(nf90_put_var(ncid_out, jm_varid, values=array_r8(istart,:), start=[jstart], count=[jend-jstart+1]))

    deallocate(lat1d)
    deallocate(start_idx)
    deallocate(array_r8)

    if (lm > 0)  then
      pfull_dimid=lm-1
      phalf_dimid=lm
      allocate(ph(lm),pf(lm-1))
      allocate(phalf(lm),pfull(lm-1))
      call get_eta_level(lm-1,p_ref,pf,ph,ak,bk,0.01)
      pfull=pf
      phalf=ph
      deallocate(ph,pf)
    endif

    !print *,'p_ref=', p_ref, 'kappa=',  kappa
    !do k=1, lm-1
    !   print *,'ak(k),bk(k),pfull(k)', ak(k),bk(k),pfull(k)
    !end do

    call check(nf90_put_var(ncid_out, pfull_varid, pfull))
    call check(nf90_put_var(ncid_out, phalf_varid, phalf))

    deallocate(ak,bk,phalf,pfull)

    !call check(nf90_put_var(ncid_out, time_varid, fhr))

  end subroutine ncio_first_call

  !=============================================================================
  !> @brief adapt MH's interpolate_data into function to perform on res files
  !> @author Xi.Chen <xi.chen@noaa.gov>
  !> @date 02/08/2016
  !> @modified for ncio output
  !> @author Mingjing.Tong <mingjing.tong@noaa.gov>
  !> @date 09/26/2019
  subroutine interp_res_ncio(da)
    !------------------------------------------------------------------!
    ! read cubed sphere data from file, interpolate to latlon,         !
    ! write latlon data to output file                                 !
    !------------------------------------------------------------------!
    use GHOST_CUBSPH_mod, only: A_grid, ghost_cubsph_update

    type(fv3_da_out_type), intent(in) :: da

    !------------------------------------------------------------------!
    ! local variables                                                  !
    !------------------------------------------------------------------!
    integer :: npx, npy, ntiles, nlon, nlat
    integer,dimension(6):: idate
    integer,dimension(30) :: voutid

    real*4 :: misval_r4
    real*8 :: misval_r8
    real*4 :: var_r4
    real*8 :: var_r8
    real*8, dimension(:), allocatable :: time
    real*4, dimension(:), allocatable :: compress_err
    real, dimension(:), allocatable :: xlon_deg, ylat_deg
    real, dimension(:), allocatable :: xlon_crs, ylat_crs, varmisval, varscale, varoffset
    real, dimension(:,:,:), allocatable :: var_latlon, var_latlon_crs, var_latlon_new
    real, dimension(:,:,:), allocatable :: ua_latlon, va_latlon
    real, dimension(:,:,:), allocatable :: ps_latlon
    real, dimension(:,:,:,:), allocatable :: var_cubsph, ua_cubsph, va_cubsph, xyz_latlon
    real, dimension(:,:,:,:), allocatable :: hydrosum_cubsph,psurf_cubsph

    real, parameter :: todeg=180./pi

    integer :: nlon_crs, nlat_crs, ntime, icoor
    integer :: i, j, k, itile, id, iv, ia, it, nx, ny, istart, istop
    integer :: k1, k2, k3, jrec, rc, itf
    integer :: status, alloc_stat, ndims, nvars, ngatts, ncid_out,      &
               time_dim, xt_dim, yt_dim, lon_dim, lat_dim,              &
               time_id, xt_id, yt_id, type, start(4), count(4), n(4),   &
               dimids(4), dimid, dimlen, lon_id, lat_id,                &
               u_id, v_id, name_len, attype, attlen, time_type,         &
               time_varid, delp_type, nhydro
    real :: delp_scale,delp_offset,delp_misval

    integer, dimension(:), allocatable :: ncid_in, vartype,             &
         varndims, varnatts, varid, nlev
    integer, dimension(:,:), allocatable :: vardimids

    logical, dimension(:), allocatable :: ignore, cs_static, do_c2l, misval

    character(len=120) :: time_name, filename, dimname, att_name, ydim_name
    character(len=16), dimension(:), allocatable :: varname

    integer, parameter :: nmdvar = 17
    character(16) :: mdvarname(nmdvar)
    integer :: nf
    integer :: oldMode
    logical :: ncflip, kflip

    data mdvarname /'ua', 'va', 'T', 'DZ',  &
                    'delp', 'sphum',  'liq_wat', 'rainwat', &
                    'ice_wat',  'snowwat',  'graupel', 'o3mr', &
                    'W', 'cld_amt', 'phy_f3d_06', 'phy_f3d_07',  &
                    'phis'/

    npx=da%npx
    npy=da%npy
    ntiles=da%ntiles
    nlon=da%nlon
    nlat=da%nlat

    !------------------------------------------------------------------!
    ! determine target grid resolution                                 !
    !------------------------------------------------------------------!
    if (finer_steps>0) then
       nlon_crs=nlon/2**finer_steps
       nlat_crs=(nlat-1)/2**finer_steps+1
    else
       nlon_crs=nlon
       nlat_crs=nlat
    endif
    im = nlon_crs
    jm = nlat_crs

    !------------------------------------------------------------------!
    ! write latlon grid                                                !
    !------------------------------------------------------------------!
    if (finer_steps==0) then
       allocate(xlon_deg(nlon_crs), ylat_deg(nlat_crs))
       xlon_deg(:)=todeg*da%xlon(:)
       ylat_deg(:)=todeg*da%ylat(:)
    else
       allocate(xlon_crs(nlon_crs), ylat_crs(nlat_crs),                 &
                xlon_deg (nlon_crs), ylat_deg (nlat_crs))
       call init_latlon_grid(xlon_crs, ylat_crs, nlon_crs, nlat_crs)
       xlon_deg(:)=todeg*xlon_crs(:)
       ylat_deg(:)=todeg*ylat_crs(:)
    endif
    if (memphis) then
       ylat_deg(   1)=0.5*(ylat_deg(   1)+ylat_deg(     2))
       ylat_deg(nlat)=0.5*(ylat_deg(nlat)+ylat_deg(nlat-1))
    endif
   
    !------------------------------------------------------------------!
    ! ncio initialize                                                !
    !-----------------------------------------------------------------!

    print*,"- WRITE ATMS DATA TO NETCDF FILE: ", trim(data_out)

    call check(nf90_create(data_out, &
               cmode=IOR(NF90_CLOBBER,NF90_NETCDF4), &
               ncid=ncid_out))

    call check(nf90_set_fill(ncid_out, NF90_NOFILL, oldMode))

    call ncio_first_call(ncid_out,nlon_crs,nlat_crs,xlon_deg,ylat_deg, &
                         voutid,time_varid,ncflip)

    print *,'done first call'

    if (allocated(xlon_deg)) deallocate(xlon_deg)
    if (allocated(ylat_deg)) deallocate(ylat_deg)
    if (allocated(xlon_crs)) deallocate(xlon_crs)
    if (allocated(ylat_crs)) deallocate(ylat_crs)

    allocate(compress_err(nvar3d+nvar2d)); compress_err=-999.

    nf=0
    do itf = 1, size(data_file)
       if (data_file(itf) /= '') nf=nf+1
    enddo

    nhydro=0

    do itf = 1, nf
       if (trim(data_file(itf)) == 'phy_data') then
         kflip = .true.
       else
         kflip = .false.
       endif
call timing_on('interp_res_cr')
       !------------------------------------------------------------------!
       ! open netcdf files with input data                                !
       !------------------------------------------------------------------!
       allocate(ncid_in(ntiles))
       do itile=1,ntiles
          write(filename,100) trim(data_file(itf)), itile
100       format(a,'.tile',i1,'.nc')
          inquire(file=filename,exist=exists)
          if (.not. exists) then
             print 101, trim(filename)
101          format("data file ",a," doesn't exist" )
             stop
          endif

          status = nf90_open(trim(filename), NF90_NOWRITE, ncid_in(itile))
          if (status /= nf90_noerr) then
             print*,"nf90_open: could not open file ",trim(filename)
             stop
          endif
       enddo
       write(filename,100) trim(data_file(itf)), 1
       !------------------------------------------------------------------!
       ! get basic info about dims and variables from first tile          !
       !------------------------------------------------------------------!
       status = nf90_inquire(ncid_in(1), ndims, nvars, ngatts, time_dim)
       status = nf90_inquire_dimension(ncid_in(1), time_dim, time_name, ntime)
       if (.not. time_unlimited) then
         ntime = 1
       endif
       status = nf90_inq_varid(ncid_in(1), trim(time_name), time_id)
       if (status /= nf90_noerr) then
          print*,"NO time variable ", trim(time_name), " in file ",trim(filename)
          stop
       endif
       status = nf90_inquire_variable(ncid_in(1), time_id, xtype=time_type)
       if (time_type /= nf90_double .and. time_type /= nf90_float) then
          print*," time variable ",trim(time_name), " has to be real"
          stop
       endif
       start(1)=1
       count(1)=ntime
       allocate(time(ntime))
       if (time_type == NF90_DOUBLE .or. time_type == NF90_FLOAT) then
         status = nf90_get_var(ncid_in(1), time_id, time, start=start, count=count)
       else
         print*," time variable ",trim(time_name), " has to be real"
         stop
       endif
       !------------------------------------------------------------------!
       ! check grid dimension and variables                               !
       ! and determine the correct dimension                              !
       !------------------------------------------------------------------!
       status = nf90_inq_dimid(ncid_in(1), "xaxis_1", xt_dim)
       if (status /= nf90_noerr) then
          print*,"NO xaxis_1 dimension in file ",trim(filename)
          stop
       endif
       status = nf90_inquire_dimension(ncid_in(1), xt_dim, len=nx)
       if (nx/=npx-1) then
          print*,"grid_xt doesn't have expected length"
          print*, nx, "instead of", npx-1
          stop
       endif
       !---------- get correct ydim
       status = nf90_inq_dimid(ncid_in(1), "yaxis_1", yt_dim)
       if (status /= nf90_noerr) then
          print*,"NO yaxis_1 dimension in file ",trim(filename)
          stop
       endif
       status = nf90_inquire_dimension(ncid_in(1), yt_dim, len=ny)
       if (ny==npy-1) then
         ydim_name = "yaxis_1"
       else
         ydim_name = "yaxis_2"
         status = nf90_inq_dimid(ncid_in(1), ydim_name, yt_dim)
         status = nf90_inquire_dimension(ncid_in(1), yt_dim,len=ny)
       endif
       if (ny/=npy-1) then
          print*,"grid_xt, grid_yt don't have expected length"
          print*, nx, ny, "instead of", npx-1, npy-1
          stop
       endif
       !------ get dim_var_ids
       status = nf90_inq_varid(ncid_in(1), "xaxis_1", xt_id)
       if (status /= nf90_noerr) then
          print*,"NO xaxis_1 variable in file ",trim(filename)
          stop
       endif
       status = nf90_inq_varid(ncid_in(1), ydim_name, yt_id)
       if (status /= nf90_noerr) then
          print*,"NO yaxis_1 variable in file ",trim(filename)
          stop
       endif
   
       !------------------------------------------------------------------!
       ! define variables and attributes                                  !
       !------------------------------------------------------------------!
       allocate(varname(nvars))
       allocate(vartype(nvars), varndims(nvars), varnatts(nvars), varid(nvars), &
                vardimids(5,nvars), nlev(nvars))
       allocate(ignore(nvars), cs_static(nvars), do_c2l(nvars))
       allocate(misval(nvars), varmisval(nvars), varscale(nvars), varoffset(nvars))

       !print *,'mdvarname(1:nvar3d)=', mdvarname(1:nvar3d)
   
       varmisval = 9.99e+20
       varoffset = 0.0
       varscale = 0.0 
       do iv=1,nvars
          status = nf90_inquire_variable(ncid_in(1), iv, name=varname(iv), &
                                         xtype=vartype(iv),ndims=varndims(iv), &
                                         dimids=vardimids(:,iv), natts=varnatts(iv))
          status = nf90_inquire_attribute(ncid_in(1), iv, trim(missing_value), attype, attlen)
          if (status == nf90_noerr) then
             if (attype==nf90_double) then 
                status = nf90_get_att(ncid_in(1), iv, trim(missing_value), misval_r8)
                varmisval(iv)=misval_r8
                misval(iv) = .true.
             elseif (attype==nf90_float) then
                status = nf90_get_att(ncid_in(1), iv, trim(missing_value), misval_r4)
                varmisval(iv)=misval_r4
                misval(iv) = .true.
             else
                misval(iv) = .false.
             endif
          else
             misval(iv) = .false.
          endif

          ! check for packing
          if (vartype(iv)==nf90_short) then
             ! get scale_factor
             status = nf90_inquire_attribute(ncid_in(1), iv, "scale_factor", attype, attlen)
             if (status == nf90_noerr) then
                if (attype==nf90_double) then 
                   status = nf90_get_att(ncid_in(1), iv, "scale_factor", misval_r8)
                   varscale(iv)=misval_r8
                elseif (attype==nf90_float) then
                   status = nf90_get_att(ncid_in(1), iv, "scale_factor", misval_r4)
                   varscale(iv)=misval_r4
                else
                   varscale(iv)=1.
                endif
             endif
             ! get add_offset
             status = nf90_inquire_attribute(ncid_in(1), iv, "add_offset", attype, attlen)
             if (status == nf90_noerr) then
                if (attype==nf90_double) then 
                   status = nf90_get_att(ncid_in(1), iv, "add_offset", misval_r8)
                   varoffset(iv)=misval_r8
                elseif (attype==nf90_float) then
                   status = nf90_get_att(ncid_in(1), iv, "add_offset", misval_r4)
                   varoffset(iv)=misval_r4
                else
                   varoffset(iv)=0.
                endif
             endif
          endif
   
          if (varndims(iv)>4) then
             ignore(iv)=.true.
             do_c2l(iv)=.false.
             cs_static(iv)=.true.
             print*, "WARNING: will ignore variable ",trim(varname(iv))
          elseif (trim(varname(iv))=='u') then
             ignore(iv)=.true.
             do_c2l(iv)=.false.
             cs_static(iv)=.true.
             print*, "WARNING: will ignore variable ",trim(varname(iv))
          elseif (trim(varname(iv))=='v') then
             ignore(iv)=.true.
             do_c2l(iv)=.false.
             cs_static(iv)=.true.
             print*, "WARNING: will ignore variable ",trim(varname(iv))
          elseif (iv==xt_id) then
             ignore(iv)=.true.
             do_c2l(iv)=.false.
             cs_static(iv)=.true.
          elseif (iv==yt_id) then
             ignore(iv)=.true.
             do_c2l(iv)=.false.
             cs_static(iv)=.true.
          else
             ignore(iv)=.false.
   
             if (vardimids(varndims(iv),iv)==time_dim) then
                cs_static(iv)=.false.
             else
                cs_static(iv)=.true.
             endif
   
             do_c2l(iv)=.false.
             if (varndims(iv)>1) then
                if (vardimids(1,iv)==xt_dim .and. vardimids(2,iv)==yt_dim) do_c2l(iv)=.true.
                if (varndims(iv)>2 .and. vardimids(3,iv)/=time_dim) then
                   status = nf90_inquire_dimension(ncid_in(1), vardimids(3,iv), len=nlev(iv))
                else
                   nlev(iv)=1
                endif
             endif
          endif
       enddo
       !------------------------------------------------------------------!
       ! check for vector quantities uname, vname                         !
       !------------------------------------------------------------------!
       status = nf90_inq_varid(ncid_in(1), trim(uname), u_id)
       if (status /= nf90_noerr) then
          u_id=0
          v_id=0
          print*,"WARNING: NO zonal flow variable", trim(uname), "found in file ",trim(filename)
       else
          status = nf90_inq_varid(ncid_in(1), trim(vname), v_id)
          if (status /= nf90_noerr) then
             u_id=0
             v_id=0
             print*,"WARNING: NO meridional flow variable ", trim(vname), " found in file ",trim(filename)
          else
             if (nlev(u_id)/=nlev(v_id) .or. vartype(u_id)/=vartype(v_id)) then
                print*,"WARNING: ", trim(uname), " and ", trim(vname),        &
                       "have different dimensions or types"
             elseif (cs_static(u_id) .or. cs_static(v_id)) then
                print*,"WARNING: ", trim(uname), " and/or ", trim(vname),        &
                       "are static arrays"
             elseif (varndims(u_id)/=4 .or. varndims(v_id)/=4) then
                print*,"WARNING: ", trim(uname), " and/or ", trim(vname),        &
                       "don't have 4 dimensions",  varndims(u_id), varndims(v_id)
             endif
          endif
       endif
       
call timing_off('interp_res_cr')
call timing_on('interp_res_cal')
       !------------------------------------------------------------------!
       ! start loop over time dependent arrays                            !
       !------------------------------------------------------------------!
       do it=1,ntime
          if (time_type == NF90_DOUBLE) then
            var_r8 = fhr-1+time(it)
            status = nf90_put_var(ncid_out, time_varid, values=var_r8)
          else if (time_type == NF90_FLOAT) then
            var_r4 = fhr-1+time(it)
            status = nf90_put_var(ncid_out, time_varid, values=var_r4)
          else
            print*," time variable ",trim(time_name), " has to be real"
            stop
          endif
          do iv=1,nvars
             if (iv==u_id .and. .not. atminc) then
                !---------------------------------------------------------!
                ! read horizontal flow                                    !
                !---------------------------------------------------------!
                if (varndims(iv)==4) then
                   start(1:4)=(/1,1,1,it/)
                   count(1:4)=(/npx-1,npy-1,nlev(iv),1/)
                   allocate(ua_cubsph(0:npx,0:npy,nlev(iv),ntiles), va_cubsph(0:npx,0:npy,nlev(iv),ntiles), &
                            stat=alloc_stat)
                   !print *,'ua_cubsph, va_cubsph, alloc_stat=', alloc_stat
                   call init_corners(ua_cubsph, nlev(iv))
                   call init_corners(va_cubsph, nlev(iv))
                   !allocate(var_latlon(nlon,nlat,nlev(iv)), stat=alloc_stat)
                   !print *,'var_latlon, alloc_stat=', alloc_stat
call timing_on('interp_res_rd')
                   call nc_get_cube_variable(ncid_in, u_id, vartype(iv), varoffset(iv), varscale(iv), &
                                             varndims(iv), npx, npy, nlev(iv), ntiles, start, count, &
                                             ua_cubsph)
                   call nc_get_cube_variable(ncid_in, v_id, vartype(iv), varoffset(iv), varscale(iv), &
                                             varndims(iv), npx, npy, nlev(iv), ntiles, start, count, &
                                             va_cubsph)
call timing_off('interp_res_rd')
                   !------------------------------------------------------!
                   ! calculate and interpolate xyz flow                   !
                   !------------------------------------------------------!
                   allocate(var_cubsph(0:npx,0:npy,nlev(iv),ntiles))
                   allocate(ua_latlon(nlon,nlat,nlev(iv)))
                   allocate(va_latlon(nlon,nlat,nlev(iv)))
                   call init_corners(var_cubsph, nlev(iv))
                   call fv3_da_out_interp_uv( &
                       da,ua_cubsph,va_cubsph,&
                       ua_latlon,va_latlon,nlev(iv), &
                       misval(iv),varmisval(iv),fill_missing)
                   deallocate(ua_cubsph, va_cubsph, var_cubsph)
                   do j = 1, nvar3d
                      if (mdvarname(j) == varname(u_id)) then
                         jrec = j
                         exit
                      end if
                   end do
                   if (finer_steps==0) then
call timing_on('interp_res_wr')
                      call nc_put_variable(ncid_out, voutid(jrec), vartype(iv),  &
                                           varoffset(iv), varscale(iv), nlon,  &
                                           nlat, nlev(iv), ncflip, kflip, ua_latlon, &
                                           ideflate, nbits, compress_err(jrec), varmisval(iv))
call timing_off('interp_res_wr')
                   else
                      allocate(var_latlon_crs(nlon_crs,nlat_crs,nlev(iv)))
                      call do_latlon_coarsening(ua_latlon, da%ylat, nlon, nlat, nlev(iv),        &
                                                var_latlon_crs, nlon_crs, nlat_crs, finer_steps, &
                                                misval(u_id), varmisval(u_id))
call timing_on('interp_res_wr')
                      call nc_put_variable(ncid_out, voutid(jrec), vartype(iv), &
                                           varoffset(iv), varscale(iv),  &
                                           nlon_crs, nlat_crs, nlev(iv), ncflip, &
                                           kflip, var_latlon_crs, &
                                           ideflate, nbits, compress_err(jrec), varmisval(iv))
call timing_off('interp_res_wr')
                      deallocate(var_latlon_crs)
                   endif
                   !------------------------------------------------------!
                   ! calculate and write meridional latlon flow           !
                   !------------------------------------------------------!
                   do j = 1, nvar3d
                      if (mdvarname(j) == varname(v_id)) then
                         jrec = j
                         exit
                      end if
                   end do
                   if (finer_steps==0) then
call timing_on('interp_res_wr')
                      call nc_put_variable(ncid_out, voutid(jrec), vartype(iv), &
                                           varoffset(iv), varscale(iv), nlon, &
                                           nlat, nlev(iv), ncflip, kflip, va_latlon, &
                                           ideflate, nbits, compress_err(jrec), varmisval(iv))
call timing_off('interp_res_wr')
                   else
                      allocate(var_latlon_crs(nlon_crs,nlat_crs,nlev(iv)))
                      call do_latlon_coarsening(va_latlon, da%ylat, nlon, nlat, nlev(iv),        &
                                                var_latlon_crs, nlon_crs, nlat_crs, finer_steps, &
                                                misval(v_id), varmisval(v_id))
call timing_on('interp_res_wr')
                      call nc_put_variable(ncid_out, voutid(jrec), vartype(iv), &
                                           varoffset(iv), varscale(iv),  &
                                           nlon_crs, nlat_crs, nlev(iv), ncflip, &
                                           kflip, var_latlon_crs, &
                                           ideflate, nbits, compress_err(jrec), varmisval(iv))
call timing_off('interp_res_wr')
                      deallocate(var_latlon_crs)
                   endif
                   deallocate(ua_latlon, va_latlon)
                endif
             elseif ((iv/=v_id .or. atminc) .and. (any(mdvarname(1:nvar3d) == varname(iv)) &
                     .or. varname(iv) == mdvarname(nmdvar))) then
                if (.not.ignore(iv) .and. .not.cs_static(iv)) then
                   if (do_c2l(iv)) then
call timing_on('interp_res_rd')
                      if (varndims(iv)==3) then
                         start(1:3)=(/1,1,it/)
                         count(1:3)=(/npx-1,npy-1,1/)
                         allocate(var_cubsph(0:npx,0:npy,nlev(iv),ntiles))
                         call init_corners(var_cubsph, nlev(iv))
                         allocate(var_latlon(nlon,nlat,nlev(iv)))
                         call nc_get_cube_variable(ncid_in, iv, vartype(iv), varoffset(iv), varscale(iv), &
                                                   varndims(iv), npx, npy, nlev(iv), ntiles, start, count, &
                                                   var_cubsph)
                         if (trim(varname(iv)) == 'phis') then     
                            var_cubsph(:,:,:,:)=var_cubsph(:,:,:,:)/grav
                         endif
                      elseif (varndims(iv)==4) then
                         start(1:4)=(/1,1,1,it/)
                         count(1:4)=(/npx-1,npy-1,nlev(iv),1/)
                         allocate(var_cubsph(0:npx,0:npy,nlev(iv),ntiles))
                         call init_corners(var_cubsph, nlev(iv))
                         allocate(var_latlon(nlon,nlat,nlev(iv)))
                         call nc_get_cube_variable(ncid_in, iv, vartype(iv), varoffset(iv), varscale(iv), &
                                                   varndims(iv), npx, npy, nlev(iv), ntiles, start, count, &
                                                   var_cubsph)
                         if (rmhydro .and. any(mdvarname(8:12) == varname(iv))) then
                            if (.not. allocated(hydrosum_cubsph)) then
                               allocate(hydrosum_cubsph(0:npx,0:npy,nlev(iv),ntiles))
                               hydrosum_cubsph=0.
                            endif
                            hydrosum_cubsph=hydrosum_cubsph + var_cubsph
                            nhydro=nhydro+1
                         endif 
                         if (trim(varname(iv)) == 'delp') then
                            if (rmhydro) then
                               if (nhydro == ncld) then
                                  var_cubsph = var_cubsph * (1. - hydrosum_cubsph)
                               else
                                  print*,"nhydro=", nhydro, "should read hydro first "
                                  stop
                               endif
                            endif
                            allocate(psurf_cubsph(0:npx,0:npy,1,ntiles))
                            psurf_cubsph = ptop
                            print *,'psurf_cubsph ptop', maxval(psurf_cubsph), minval(psurf_cubsph) 
                            do itile=1,ntiles
                               do j=0,npy
                                  do i=0,npx
                                     do k=nlev(iv),1,-1
                                        psurf_cubsph(i,j,1,itile)= psurf_cubsph(i,j,1,itile) + var_cubsph(i,j,k,itile)
                                     end do
                                  end do
                               end do
                            end do
                            if (pseudo_ps) then
                               psurf_cubsph= (psurf_cubsph/stndrd_atmos_ps)**(rdgas/grav*stndrd_atmos_lapse)  
                            endif
                         endif
                      else
                         print*," unexpected number of dimensions (", varndims(iv), &
                                ") for variable ",trim(varname(iv))
                         stop
                      endif
call timing_off('interp_res_rd')
                      
                      call fv3_da_out_interp_scalar( &
                          da,var_cubsph,var_latlon,nlev(iv), &
                          misval(iv),varmisval(iv),fill_missing)

                      if (trim(varname(iv)) == 'delp') then
                         allocate(ps_latlon(nlon,nlat,1))
                         call fv3_da_out_interp_scalar( &
                              da,psurf_cubsph,ps_latlon,1, &
                              misval(iv),varmisval(iv),fill_missing)
                         if (pseudo_ps) then
                            ps_latlon = ps_latlon**(grav2/(rdgas*stndrd_atmos_lapse))*stndrd_atmos_ps
                         endif
                         deallocate(psurf_cubsph)
                      endif
                   
                      if (finer_steps==0) then
                         if (varndims(iv)==3 .and. trim(varname(iv)) == 'phis' ) then
                            jrec = nvar3d + 2
call timing_on('interp_res_wr')
                            call nc_put_variable(ncid_out, voutid(jrec), vartype(iv), &
                                                 varoffset(iv), varscale(iv), nlon, &
                                                 nlat, 1, ncflip, kflip, var_latlon(:,:,1), &
                                                 ideflate, nbits, compress_err(jrec), varmisval(iv))
call timing_off('interp_res_wr')
                         else
                            if (trim(varname(iv)) == 'delp') then
                               delp_scale=varscale(iv)
                               delp_offset=varoffset(iv)
                               delp_type=vartype(iv)
                               delp_misval=varmisval(iv)
                            endif
call timing_on('interp_res_wr')
                            do j = 1, nvar3d
                               if (mdvarname(j) == varname(iv)) then
                                  jrec = j
                                  exit
                               end if
                            end do
                            call nc_put_variable(ncid_out, voutid(jrec), vartype(iv), &
                                                 varoffset(iv), varscale(iv), nlon, &
                                                 nlat, nlev(iv), ncflip, kflip, var_latlon, &
                                                 ideflate, nbits, compress_err(jrec), varmisval(iv))
call timing_off('interp_res_wr')
                         endif
                      else
                         allocate(var_latlon_crs(nlon_crs,nlat_crs,nlev(iv)))
                         call do_latlon_coarsening(var_latlon, da%ylat, nlon, nlat, nlev(iv),  &
                              var_latlon_crs, nlon_crs, nlat_crs, finer_steps, &
                              misval(iv), varmisval(iv))
                         if (varndims(iv)==3 .and. trim(varname(iv)) == 'phis') then
                            allocate(var_latlon_new(nlon_crs,nlat_crs,1))
                            jrec = nvar3d + 2
                            var_latlon_new(:,:,1)=var_latlon_crs(:,:,1)/grav
call timing_on('interp_res_wr')
                            call nc_put_variable(ncid_out, voutid(jrec), vartype(iv), &
                                                 varoffset(iv), varscale(iv), nlon, &
                                                 nlat, 1, ncflip, kflip, var_latlon_new, &
                                                 ideflate, nbits, compress_err(jrec), varmisval(iv))
                            deallocate(var_latlon_new)
call timing_off('interp_res_wr')
                         else
                            if (trim(varname(iv)) == 'delp') then
                               delp_scale=varscale(iv)
                               delp_offset=varoffset(iv)
                               delp_type=vartype(iv)
                               delp_misval=varmisval(iv)
                            endif
call timing_on('interp_res_wr')
                            do j = 1, nvar3d
                               if (mdvarname(j) == varname(iv)) then
                                  jrec = j
                                  exit
                               end if
                            end do
                            call nc_put_variable(ncid_out, voutid(jrec), vartype(iv), &
                                                 varoffset(iv), varscale(iv), nlon_crs, &
                                                 nlat_crs, nlev(iv), ncflip, kflip, &
                                                 var_latlon_crs, &
                                                 ideflate, nbits, compress_err(jrec), varmisval(iv))
call timing_off('interp_res_wr')
                         endif
                         deallocate(var_latlon_crs)
                      endif
                      deallocate(var_latlon, var_cubsph)
                   endif
                endif
             endif
          enddo
       enddo

       do itile=1,ntiles
          status = nf90_close(ncid_in(itile))
       enddo
call timing_off('interp_res_cal')
    
       deallocate(ncid_in)
       deallocate(time)
   
       deallocate(varname, vartype, varndims, varnatts, varid, vardimids, nlev, ignore,  &
                  cs_static, do_c2l, misval, varmisval, varscale, varoffset)

       print 102, trim(data_file(itf))//".tile?.nc",trim(data_out)
102 format(/,"interpolation done for data in ",a,/,"interpolated data stored in ",a,/)

    end do ! do itf=1,size(date_file)

! put ps
    if (nvar2d > 0) then
       do it=1,ntime
          jrec = nvar3d + 1
          call nc_put_variable(ncid_out, voutid(jrec), delp_type, &
                               delp_offset, delp_scale, im, jm, 1, &
                               ncflip, kflip, ps_latlon, &
                               ideflate, nbits, compress_err(jrec), delp_misval)
          deallocate(ps_latlon) 
       end do
    endif

    if (allocated(hydrosum_cubsph)) deallocate(hydrosum_cubsph)

    if (ideflate > 0 .and. nbits > 0) then
       status = nf90_redef(ncid=ncid_out)
       do i=1, nvar3d+nvar2d
          if (compress_err(i) > 0) then
             status = nf90_put_att(ncid_out, voutid(i), 'max_abs_compression_error', compress_err(i))
             status = nf90_put_att(ncid_out, voutid(i), 'nbits', nbits)
          endif
       enddo
       status = nf90_enddef(ncid=ncid_out)
    endif

    call check(nf90_close(ncid_out))

    contains
      subroutine init_corners(var, nz)
        integer, intent(in) :: nz
        real, dimension(0:npx,0:npy,nz,ntiles), intent(inout) :: var

        var(  0,  0,:,:)=0.
        var(  0,npy,:,:)=0.
        var(npx,npy,:,:)=0.
        var(npx,  0,:,:)=0.
      end subroutine init_corners

  end subroutine interp_res_ncio

      subroutine get_eta_level(npz, p_s, pf, ph, ak, bk, pscale)

        integer, intent(in) :: npz
        real, intent(in)  :: p_s            !< unit: pascal
        real, intent(in)  :: ak(npz+1)
        real, intent(in)  :: bk(npz+1)
        real, intent(in), optional :: pscale
        real, intent(out) :: pf(npz)
        real, intent(out) :: ph(npz+1)
        integer k

        ph(1) = ak(1)
        do k=2,npz+1
           ph(k) = ak(k) + bk(k)*p_s
        enddo

        if ( present(pscale) ) then
            do k=1,npz+1
               ph(k) = pscale*ph(k)
            enddo
        endif

        if( ak(1) > 1.E-8 ) then
           pf(1) = (ph(2) - ph(1)) / log(ph(2)/ph(1))
        else
           pf(1) = (ph(2) - ph(1)) * kappa/(kappa+1.)
        endif

        do k=2,npz
           pf(k) = (ph(k+1) - ph(k)) / log(ph(k+1)/ph(k))
        enddo

      end subroutine get_eta_level

      function get_time_units_from_idate(idate, time_measure) result(time_units)
          ! create time units attribute of form 'hours since YYYY-MM-DD
          ! HH:MM:SS'
          ! from integer array with year,month,day,hour,minute,second
          ! optional argument 'time_measure' can be used to change 'hours' to
          ! 'days', 'minutes', 'seconds' etc.
          character(len=*), intent(in), optional :: time_measure
          integer, intent(in) ::  idate(6)
          character(len=12) :: timechar
          character(len=nf90_max_name) :: time_units
          if (present(time_measure)) then
             timechar = trim(time_measure)
          else
             timechar = 'hours'
          endif
          write(time_units,101) idate
101       format(' since ',i4.4,'-',i2.2,'-',i2.2,' ',&
          i2.2,':',i2.2,':',i2.2)
          time_units = trim(adjustl(timechar))//time_units
      end function get_time_units_from_idate

      subroutine nc_put_attribute(ncid, varid, reclongname, recunit)

        integer, intent(in) :: ncid, varid
        character(len=120), intent(in) :: reclongname, recunit
        integer :: ncerr

        ncerr = nf90_put_att(ncid, varid, "cell_methods", "time: point")
        ncerr = nf90_put_att(ncid, varid, "long_name", trim(reclongname))
        ncerr = nf90_put_att(ncid, varid, "output_file","atm")
        ncerr = nf90_put_att(ncid, varid, "units", trim(recunit))

      end subroutine nc_put_attribute

end module interp_res_ncio_mod



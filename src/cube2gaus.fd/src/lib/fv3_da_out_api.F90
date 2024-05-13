!-------------------------------------------------------------------------------
!> @brief FV3 DA outbound lib API
!> @author Xi.Chen <xi.chen@noaa.gov>
!> @date 02/23/2016
!
!  REVISION HISTORY:
!  02/23/2016 - Initial Version
!-------------------------------------------------------------------------------

module fv3_da_out_api_mod

  use GHOST_CUBSPH_mod, only: A_grid, ghost_cubsph_update
  use CUB2LATLON_mod, only: init_cubsph_grid, init_latlon_grid,         &
                            read_c2l_weight,  write_c2l_weight,         &
                            get_c2l_weight,   interpolate_data,         &
                            read_grid_dimensions, &
                            init_latlon_grid, &
                            do_c2l_interpolation, &
                            do_latlon_coarsening

  use fv3_da_ctrl_mod, only: read_gaus_dimensions, &
                             read_gaus_grid,grid_file,ntiles,get_weights_only

  use fv_timing_mod, only: timing_on, timing_off, timing_prt
  use netcdf
  use io_nc_util_mod, only: check

  implicit none
  private

  public :: fv3_da_out_type
  public :: fv3_da_out_init
  public :: fv3_da_out_end
  public :: fv3_da_out_interp_scalar
  public :: fv3_da_out_interp_uv

  type fv3_da_out_type
    integer :: npx, npy, ntiles, nlon, nlat
    real, dimension(:,:,:,:), allocatable :: sph_corner
    real, dimension(:),       allocatable :: xlon, ylat
    real,    dimension(:,:,:), allocatable :: c2l_weight
    integer, dimension(:,:,:), allocatable :: c2l_index
    real,    dimension(:,:,:,:), allocatable  :: elon_cubsph, elat_cubsph
    real,    dimension(:,:,:),   allocatable  :: elon_latlon, elat_latlon
  end type fv3_da_out_type

contains
  !=============================================================================
  !> @brief alloc and init fv3_da_out_type storage
  !> @author Xi.Chen <xi.chen@noaa.gov>
  !> @date 02/23/2016
  subroutine fv3_da_out_init(da)
    type(fv3_da_out_type), intent(out) :: da
    ! local
    integer :: npx, npy, nlon, nlat

    !--------------------------------------------------------------------!
    ! initialize cubed sphere grid                                       !
    !--------------------------------------------------------------------!
    call read_grid_dimensions(grid_file, npx, npy)
    print *,'grid_file', grid_file
    print *,'ntiltes, npx, npy', ntiles, npx, npy
    da%npx = npx
    da%npy = npy
    da%ntiles = ntiles
    allocate(da%sph_corner(2,0:npx+1,0:npy+1,ntiles))
    call init_cubsph_grid(npx, npy, ntiles, grid_file, da%sph_corner)
    !--------------------------------------------------------------------!
    ! initialize Gaussian grid                                           !
    !--------------------------------------------------------------------!
    call read_gaus_dimensions(nlon, nlat)
    da%nlon = nlon
    da%nlat = nlat
    allocate(da%xlon(nlon), da%ylat(nlat))
    call read_gaus_grid(da%xlon, da%ylat, nlon, nlat)
    !--------------------------------------------------------------------!
    ! calculate weights for bilinear interpolation                       !
    ! from cubed sphere to latlon grid                                   !
    !--------------------------------------------------------------------!
    allocate(da%c2l_index(3,nlon,nlat),da%c2l_weight(4,nlon,nlat))
    allocate(da%elon_cubsph(3,0:npx,0:npy,ntiles), &
             da%elat_cubsph(3,0:npx,0:npy,ntiles), &
             da%elon_latlon(3,nlon,nlat), &
             da%elat_latlon(3,nlon,nlat) &
             )
    call timing_on('get_weights')
    call get_c2l_weight(da%sph_corner, npx, npy, ntiles, &
                        da%xlon, da%ylat, nlon, nlat,  &
                        da%c2l_index, da%c2l_weight, &
                        da%elon_cubsph, da%elat_cubsph, &
                        da%elon_latlon, da%elat_latlon)
    call timing_off('get_weights')

    if (get_weights_only) then
      call write_weight_nc(da)
    endif

  end subroutine fv3_da_out_init
  !=============================================================================
  !> @brief do interpolation from cubed-sphere to Gaussian grid (scalar)
  !> @author Xi.Chen <xi.chen@noaa.gov>
  !> @date 02/23/2016
  subroutine fv3_da_out_interp_scalar(da,var_cubsph,var_latlon,nlev, &
      misval,varmisval,fill_missing)
    type(fv3_da_out_type), intent(in) :: da
    integer, intent(in) :: nlev
    real, dimension(0:da%npx,0:da%npy,1:nlev,da%ntiles), intent(inout) :: &
        var_cubsph
    real, dimension(da%nlon, da%nlat, nlev), intent(inout) :: var_latlon
    logical, intent(in) :: misval,fill_missing
    real, intent(in) :: varmisval
    ! local
    integer :: itile

    do itile=1,da%ntiles
      call ghost_cubsph_update(var_cubsph, 0, da%npx, 0, da%npy, &
                               nlev, 1, da%ntiles,  &
                               1, nlev, itile, A_grid)
      call do_c2l_interpolation(var_cubsph(:,:,:,itile), 0, da%npx, &
                                0, da%npy, nlev, itile, nlev, &
                                var_latlon, da%nlon, da%nlat, &
                                da%c2l_index, da%c2l_weight,  &
                                misval, varmisval, fill_missing)
    enddo


  end subroutine fv3_da_out_interp_scalar
  !=============================================================================
  !> @brief do interpolation from cubed-sphere to Gaussian grid (u,v wind)
  !> @author Xi.Chen <xi.chen@noaa.gov>
  !> @date 02/24/2016
  subroutine fv3_da_out_interp_uv( &
      da,ua_cubsph,va_cubsph, &
      ua_latlon,va_latlon,nlev, &
      misval,varmisval,fill_missing)
    type(fv3_da_out_type), intent(in) :: da
    integer, intent(in) :: nlev
    real, dimension(0:da%npx,0:da%npy,1:nlev,da%ntiles), intent(inout) :: &
        ua_cubsph,va_cubsph
    real, dimension(da%nlon, da%nlat, nlev), intent(inout) :: &
        ua_latlon,va_latlon
    logical, intent(in) :: misval,fill_missing
    real, intent(in) :: varmisval
    ! local
    !real, dimension(0:da%npx,0:da%npy,1:nlev,da%ntiles) :: var_cubsph
    !real, dimension(da%nlon, da%nlat, nlev, 3) :: xyz_latlon
    real, dimension(:,:,:,:), allocatable :: var_cubsph
    real, dimension(:,:,:,:), allocatable :: xyz_latlon
    integer :: itile,icoor,i,j,k,alloc_stat

    allocate(var_cubsph(0:da%npx,0:da%npy,1:nlev,da%ntiles),stat=alloc_stat)
    !print *,'alloc_stat=', alloc_stat

    call init_corners(var_cubsph, nlev)
    do icoor=1,3
       do itile=1,da%ntiles
          call timing_on('kloop_uv_cub')
          do k=1,nlev
             do j=1,da%npy-1
                do i=1,da%npx-1
                   var_cubsph(i,j,k,itile)=ua_cubsph(i,j,k,itile) &
                                          *da%elon_cubsph(icoor,i,j,itile) &
                                          +va_cubsph(i,j,k,itile) &
                                          *da%elat_cubsph(icoor,i,j,itile)
                enddo
             enddo
          enddo
          call timing_off('kloop_uv_cub')
       enddo
       allocate(xyz_latlon(da%nlon, da%nlat, nlev, 3),stat=alloc_stat)
       call fv3_da_out_interp_scalar( &
           da,var_cubsph,xyz_latlon(:,:,:,icoor),nlev, &
           misval,varmisval,fill_missing)
    enddo

    !------------------------------------------------------!
    ! calculate and write zonal latlon flow                !
    !------------------------------------------------------!
    call timing_on('kloop_uv_latlon')
    do k=1,nlev
       do j=1,da%nlat
          do i=1,da%nlon
             ua_latlon(i,j,k)=xyz_latlon(i,j,k,1)*da%elon_latlon(1,i,j) &
                             +xyz_latlon(i,j,k,2)*da%elon_latlon(2,i,j) &
                             +xyz_latlon(i,j,k,3)*da%elon_latlon(3,i,j)
          enddo
       enddo
    enddo
    do k=1,nlev
       do j=1,da%nlat
          do i=1,da%nlon
             va_latlon(i,j,k)=xyz_latlon(i,j,k,1)*da%elat_latlon(1,i,j) &
                             +xyz_latlon(i,j,k,2)*da%elat_latlon(2,i,j) &
                             +xyz_latlon(i,j,k,3)*da%elat_latlon(3,i,j)
          enddo
       enddo
    enddo

    deallocate(var_cubsph,xyz_latlon)

    contains
      subroutine init_corners(var, nz)
        integer, intent(in) :: nz
        real, dimension(0:da%npx,0:da%npy,nz,da%ntiles), intent(inout) :: var

        var(     0,     0,:,:)=0.
        var(     0,da%npy,:,:)=0.
        var(da%npx,da%npy,:,:)=0.
        var(da%npx,     0,:,:)=0.
      end subroutine init_corners
  end subroutine fv3_da_out_interp_uv
  !=============================================================================
  !> @brief clean up the storage
  !> @author Xi.Chen <xi.chen@noaa.gov>
  !> @date 02/23/2016
  subroutine fv3_da_out_end(da)
    type(fv3_da_out_type), intent(inout) :: da

    deallocate(da%sph_corner, da%xlon, da%ylat, da%c2l_index, da%c2l_weight)

    deallocate(da%elon_cubsph, da%elat_cubsph, da%elon_latlon, da%elat_latlon)

  end subroutine fv3_da_out_end
  !=============================================================================
  !> @brief clean up the storage
  !> @author Mingjing.Tong <mingjing.tong@noaa.gov>
  !> @date 03/05/2020

  subroutine write_weight_nc(da)
    type(fv3_da_out_type), intent(inout) :: da
    character(len=128)             :: outfile
    integer                        :: ncid_out
    integer :: npx_dimid, npy_dimid, nlon_dimid, nlat_dimid
    integer :: ncoor_dimid, nweight_dimid, ntile_dimid
    integer :: xlon_varid, ylat_varid, c2lidx_varid, c2lwgt_varid
    integer :: elonc_varid, elatc_varid, elonl_varid, elatl_varid
    integer :: oldMode
    character(len=30) :: case
 
    if (da%npx < 100) then
      write (case, "(A1,I2)") "C", da%npx-1
    else if (da%npx < 1000) then
      write (case, "(A1,I3)") "C", da%npx-1
    else
      write (case, "(A1,I4)") "C", da%npx-1
    endif
    print *, 'case', trim(case)
    outfile="./c2g_weight_"//trim(case)// ".nc"
    call check(nf90_create(outfile, &
               cmode=IOR(IOR(NF90_CLOBBER,NF90_NETCDF4), NF90_CLASSIC_MODEL), & 
               ncid=ncid_out))

    call check(nf90_set_fill(ncid_out, NF90_NOFILL, oldMode))

    !--- define dimension
    call check(nf90_def_dim(ncid_out, "npx", da%npx+1, npx_dimid))
    call check(nf90_def_dim(ncid_out, "npy", da%npy+1, npy_dimid))
    call check(nf90_def_dim(ncid_out, "nlon", da%nlon, nlon_dimid))
    call check(nf90_def_dim(ncid_out, "nlat", da%nlat, nlat_dimid))
    call check(nf90_def_dim(ncid_out, "ncoor", 3, ncoor_dimid))
    call check(nf90_def_dim(ncid_out, "nweight", 4, nweight_dimid))
    call check(nf90_def_dim(ncid_out, "ntile", da%ntiles, ntile_dimid))

    !--- define field
    call check(nf90_def_var(ncid_out, "xlon", NF90_DOUBLE, nlon_dimid, xlon_varid))
    call check(nf90_put_att(ncid_out, xlon_varid, "long_name", "T-cell longitude"))

    call check(nf90_def_var(ncid_out, "ylat", NF90_DOUBLE, nlat_dimid, ylat_varid))
    call check(nf90_put_att(ncid_out, ylat_varid, "long_name", "T-cell latitude"))

    call check(nf90_def_var(ncid_out, "c2l_index", NF90_INT, &
                       (/ncoor_dimid,nlon_dimid,nlat_dimid/), c2lidx_varid))
    call check(nf90_put_att(ncid_out, c2lidx_varid, "long_name", "c2l index"))

    call check(nf90_def_var(ncid_out, "c2l_weight", nf90_float, &
                       (/nweight_dimid,nlon_dimid,nlat_dimid/), c2lwgt_varid))
    call check(nf90_put_att(ncid_out, c2lwgt_varid, "long_name", "c2l weights"))

    call check(nf90_def_var(ncid_out, "elon_cubsph", nf90_float, &
                       (/ncoor_dimid,npx_dimid,npy_dimid,ntile_dimid/), elonc_varid))
    call check(nf90_put_att(ncid_out, elonc_varid, "long_name", "cubed-sphere elon"))

    call check(nf90_def_var(ncid_out, "elat_cubsph", nf90_float, &
                       (/ncoor_dimid,npx_dimid,npy_dimid,ntile_dimid/), elatc_varid))
    call check(nf90_put_att(ncid_out, elatc_varid, "long_name", "cubed-sphere elat"))

    call check(nf90_def_var(ncid_out, "elon_latlon", nf90_float, &
                       (/ncoor_dimid,nlon_dimid,nlat_dimid/), elonl_varid))
    call check(nf90_put_att(ncid_out, elonl_varid, "long_name", "lat-lon elon"))

    call check(nf90_def_var(ncid_out, "elat_latlon", nf90_float, &
                       (/ncoor_dimid,nlon_dimid,nlat_dimid/), elatl_varid))
    call check(nf90_put_att(ncid_out, elatl_varid, "long_name", "lat-lon elat"))

    call check(nf90_enddef(ncid=ncid_out))

    !--- put fields

    call check(nf90_put_var(ncid_out, xlon_varid, da%xlon))
    call check(nf90_put_var(ncid_out, ylat_varid, da%ylat))

    call check(nf90_put_var(ncid_out, c2lidx_varid, da%c2l_index))
    call check(nf90_put_var(ncid_out, c2lwgt_varid, da%c2l_weight))
  
    call check(nf90_put_var(ncid_out, elonc_varid, da%elon_cubsph)) 
    call check(nf90_put_var(ncid_out, elatc_varid, da%elat_cubsph))
  
    call check(nf90_put_var(ncid_out, elonl_varid, da%elon_latlon))
    call check(nf90_put_var(ncid_out, elatl_varid, da%elat_latlon))

    call check(nf90_close(ncid_out))

  end subroutine write_weight_nc
  !=============================================================================
end module fv3_da_out_api_mod


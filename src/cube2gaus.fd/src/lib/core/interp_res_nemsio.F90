!-------------------------------------------------------------------------------
!> @brief interpolate the restart files
!>        adapted from MH's interpolate_data subroutine
!> @author Xi.Chen <xi.chen@noaa.gov>
!> @date 02/08/2016
!
!  REVISION HISTORY:
!  02/08/2016 - Initial Version
!-------------------------------------------------------------------------------

module interp_res_nemsio_mod

  use cub2latlon_mod, only: init_latlon_grid, &
                            do_c2l_interpolation, &
                            do_latlon_coarsening
  use fv_timing_mod, only: timing_on, timing_off, timing_prt
  use fv3_da_out_api_mod, only: fv3_da_out_type, &
                                fv3_da_out_interp_scalar, &
                                fv3_da_out_interp_uv
  use netcdf
  use nemsio_module
  use fv3_da_ctrl_mod, only: netcdf_err, nc_get_cube_variable
  use fv3_da_ctrl_mod, only: finer_steps, nvar3dout,                   &
                        read_res, write_res, memphis, write_nemsio,    &
                        grid_file, data_file, data_out,                &
                        uname, vname, missing_value,                   &
                        fill_missing,write_nemsioflip,ideflate, nbits, &
                        rmhydro, pseudo_ps, get_weights_only,atminc,   &
                        gaus_file, yy, mm, dd, hh, fhr

  implicit none
  private
  integer :: im, jm, mlev, nmeta, nsoil, ncld, ntrac, nrec
  integer :: idate(8)
  integer :: nvar3d
  integer :: idrt, idsl, idvc, idvm
  integer, parameter :: nmetaaryi=0
  integer, parameter :: nmetavari=1
  integer, parameter :: nmetavarr=0
  integer, parameter :: nmetavarc=3
  integer :: varival(nmetavari)
  integer,dimension(:),allocatable        :: reclev

  character(16),dimension(:),allocatable  :: recname,reclevtyp
  character(16)              :: variname(nmetavari), varcname(nmetavarc)
  character(16)              :: varcval(nmetavarc)

  real(4), dimension(:,:,:), allocatable  :: vcoord
  real(4), dimension(:), allocatable      :: lon1d, lat1d
  real(4) :: ptop
  logical :: exists

  data variname /'ncnsto'/
  data varival /8/

  public :: interp_res_nemsio

  real, parameter :: pi = 3.141592653589793
  real, parameter :: grav = 9.80665

contains

!----------------------------------------------------------------------------------------
  subroutine nemsio_first_call(im,jm,lon,lat)

    implicit none

    integer, intent(in)                 :: im,jm
    real(4), intent(in)                 :: lon(im), lat(jm)
!
!** local vars
    integer i, j, k, lm, lunit, rc, error
    integer, parameter :: nvar2d = 2
    integer, parameter :: nrec_all=18
    integer :: ncid, id_dim, id_var, vartype
    integer :: ntot3d

    real(4), allocatable   :: dummy_r4(:)
    real(8), allocatable   :: dummy_r8(:)
    real(4), dimension(:), allocatable   :: ak, bk

    character(16) :: recname_all(nrec_all)

    data recname_all /'ugrd', 'vgrd', 'dzdt', 'delz', &
                  'tmp', 'dpres', 'spfh',  'clwmr',   &
                  'rwmr', 'icmr',  'snmr',  'grle',   &
                  'o3mr', 'cld_amt', 'cnvw', 'cnvc',  &
                  'pres', 'hgt'/

    idate = 0
    idate(1) = yy
    idate(2) = mm
    idate(3) = dd
    idate(4) = hh

!** open fv_core.res.nc

    error=nf90_open("./fv_core.res.nc",nf90_nowrite,ncid)
    call netcdf_err(error, 'OPENING FILE' )

    error=nf90_inq_dimid(ncid, 'xaxis_1', id_dim)
    call netcdf_err(error, 'READING xaxis_1' )
    error=nf90_inquire_dimension(ncid,id_dim,len=lm)
    call netcdf_err(error, 'READING xaxis_1' )

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
       error = nf90_inq_varid (ncid, 'bk', id_var)
       call netcdf_err(error, 'READING bk ID' )
       error=nf90_get_var(ncid, id_var, dummy_r8)
       call netcdf_err(error, 'READING bk' )
       bk=dummy_r8
       deallocate(dummy_r8)
    else
       allocate(dummy_r4(lm))
       error=nf90_get_var(ncid, id_var, dummy_r4)
       call netcdf_err(error, 'READING ak' )
       ak=dummy_r4
       error = nf90_inq_varid (ncid, 'bk', id_var)
       call netcdf_err(error, 'READING bk ID' )
       error=nf90_get_var(ncid, id_var, dummy_r4)
       call netcdf_err(error, 'READING bk' )
       bk=dummy_r4
       deallocate(dummy_r4)
    end if

    if (lm > 0)  then
      if(.not.allocated(vcoord))  allocate(vcoord(lm,3,2))
      vcoord = 0.
    endif

    if (write_nemsioflip) then
       do k=1,lm
          vcoord(k,1,1) = ak(lm-k+1)
          vcoord(k,2,1) = bk(lm-k+1)
       enddo
    else
       vcoord(:,1,1) = ak(:)
       vcoord(:,2,1) = bk(:)
    end if

    ptop = ak(1)

    print *,'ptop=', ptop

!** get lat lon:
    if(.not.allocated(lon1d)) allocate(lon1d(im*jm))
    if(.not.allocated(lat1d)) allocate(lat1d(im*jm))

    if (write_nemsioflip) then
       do j=1,jm
         do i=1,im
           lon1d((j-1)*im+i) = lon(i)
           lat1d((j-1)*im+i) = lat(jm-j+1)
         enddo
       enddo
    else
       do j=1,jm
         do i=1,im
           lon1d((j-1)*im+i) = lon(i)
           lat1d((j-1)*im+i) = lat(j)
         enddo
       enddo
    end if

    ntrac=9
    ncld=5
    nsoil=4
    ntot3d=6
    inquire(file='control.dat',exist=exists)
    if(exists)then
!** open control.dat
      lunit=30
      open( lunit, file='control.dat', form='unformatted',status='old' )
      read (lunit) ntrac,ncld,nsoil,ntot3d
      close(lunit)
    end if 
    if (ntot3d == 7) then
       nvar3d=16
    else
       nvar3d=15
    end if
    if (nvar3dout <= nvar3d) nvar3d=nvar3dout

!** get record info
    mlev = lm - 1
    nrec = nvar3d * mlev + nvar2d

    if(.not. allocated(recname))allocate(recname(nrec))
    if(.not. allocated(reclevtyp))allocate(reclevtyp(nrec))
    if(.not. allocated(reclev))allocate(reclev(nrec))
    reclevtyp(1:nvar3d*mlev)='mid layer'
    reclevtyp(nvar3d*mlev+1:nrec)='sfc'
    k=0
    do j = 1, nvar3d
       do i = 1, mlev
          k=k+1
          recname(k) = recname_all(j)
          reclev(k)=i
       end do
    end do
    reclev(nvar3d*mlev+1:nrec)=1
    recname(nvar3d*mlev+1:nrec)=recname_all(17:18)

    nmeta = 8
    idrt  = 4

    idsl = 1
    idvc = 2
    idvm = 1

    k = 1
    varcname(k) = 'hydrostatic'
    varcval(k) = 'non-hydrostatic'
    if(write_nemsioflip) then
      k=k+1
      varcname(k) = 'y-direction'
      varcval(k) = 'north2south'
      k=k+1
      varcname(k) = 'z-direction'
      varcval(k) = 'bottom2top'
    else
      k=k+1
      varcname(k) = 'y-direction'
      varcval(k) = 'south2north'
      k=k+1
      varcname(k) = 'z-direction'
      varcval(k) = 'top2bottom'
    endif

      !print *,'first call idate=',idate,'nfour=',nf_hours,nf_minutes,nf_seconds, &
      !'dim=',im,jm,lm,'nmeta=',nmeta,'idrt=',idrt,'nsoil=',nsoil,            &
      !'ntrac=',ntrac,'nrec=',nrec,            &
      !'vcoord=',vcoord(1:5,1,1),'nfhours=',nf_hours,nf_minutes,nfseconds,nfsecond_den, &
      !'idsl=',idsl,'idvc=',idvc,idvm

  end subroutine nemsio_first_call

  !=============================================================================
  !> @brief adapt MH's interpolate_data into function to perform on res files
  !> @author Xi.Chen <xi.chen@noaa.gov>
  !> @date 02/08/2016
  !> @modified for nemsio output
  !> @author Mingjing.Tong <mingjing.tong@noaa.gov>
  !> @date 09/26/2019
  subroutine interp_res_nemsio(da)
    !------------------------------------------------------------------!
    ! read cubed sphere data from file, interpolate to latlon,         !
    ! write latlon data to output file                                 !
    !------------------------------------------------------------------!
    use GHOST_CUBSPH_mod, only: A_grid, ghost_cubsph_update

#include "netcdf.inc"
    type(fv3_da_out_type), intent(in) :: da

    !------------------------------------------------------------------!
    ! local variables                                                  !
    !------------------------------------------------------------------!
    integer :: npx, npy, ntiles, nlon, nlat
    real*4 :: misval_r4
    real*8 :: misval_r8
    real*4, dimension(:), allocatable :: xlon_r4, ylat_r4, tmp
    real*8, dimension(:), allocatable :: time
    real*4, dimension(:,:), allocatable :: arrayr4,psr4
    real*4, dimension(:,:,:), allocatable :: var_r4
    real*8, dimension(:,:,:), allocatable :: var_r8
    integer*2, dimension(:,:,:), allocatable :: var_i2
    real, dimension(:), allocatable :: xlon_crs, ylat_crs, varmisval, varscale, varoffset
    real, dimension(:,:,:), allocatable :: var_latlon, var_latlon_crs
    real, dimension(:,:,:), allocatable :: ua_latlon, va_latlon
    real, dimension(:,:,:,:), allocatable :: var_cubsph, ua_cubsph, va_cubsph, xyz_latlon

    real, parameter :: todeg=180./pi

    integer :: nlon_crs, nlat_crs, ntime, icoor
    integer :: i, j, k, itile, id, iv, ia, it, nx, ny, istart, istop
    integer :: k1, k2, k3, jrec, rc, itf
    integer :: status, alloc_stat, ndims, nvars, ngatts,                &
               time_dim, xt_dim, yt_dim, lon_dim, lat_dim,              &
               time_id, xt_id, yt_id, type, start(4), count(4), n(4),   &
               dimids(4), dimid, dimlen, lon_id, lat_id,                &
               u_id, v_id, name_len, attype, attlen, time_type

    integer, dimension(:), allocatable :: ncid_in, vartype,             &
         varndims, varnatts, varid, nlev
    integer, dimension(:,:), allocatable :: vardimids

    logical, dimension(:), allocatable :: ignore, cs_static, do_c2l, misval

    character(len=120) :: time_name, filename, dimname, att_name, ydim_name
    character(len=120), dimension(:), allocatable :: varname

    integer, parameter :: nmdvar = 17
    character(16) :: mdvarname(nmdvar)
    character(16) :: nvname,nvlevtyp
    integer :: nvlev,nf

    type(nemsio_gfile)    :: nemsiofile

    data mdvarname /'ua', 'va', 'W', 'DZ', &
                    'T', 'delp', 'sphum',  'liq_wat',   &
                    'rainwat', 'ice_wat',  'snowwat',  'graupel',   &
                    'o3mr', 'cld_amt', 'phy_f3d_06', 'phy_f3d_07',  &
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
       allocate(xlon_r4(nlon_crs), ylat_r4(nlat_crs))
       xlon_r4(:)=todeg*da%xlon(:)
       ylat_r4(:)=todeg*da%ylat(:)
    else
       allocate(xlon_crs(nlon_crs), ylat_crs(nlat_crs),                 &
                xlon_r4 (nlon_crs), ylat_r4 (nlat_crs))
       call init_latlon_grid(xlon_crs, ylat_crs, nlon_crs, nlat_crs)
       xlon_r4(:)=todeg*xlon_crs(:)
       ylat_r4(:)=todeg*ylat_crs(:)
    endif
    if (memphis) then
       ylat_r4(   1)=0.5*(ylat_r4(   1)+ylat_r4(     2))
       ylat_r4(nlat)=0.5*(ylat_r4(nlat)+ylat_r4(nlat-1))
    endif
   
    !------------------------------------------------------------------!
    ! nemsio initialize                                                !
    !------------------------------------------------------------------!

    call nemsio_init(iret=rc)


    call nemsio_first_call(nlon_crs,nlat_crs,xlon_r4,ylat_r4)

    if (allocated(xlon_r4)) deallocate(xlon_r4)
    if (allocated(ylat_r4)) deallocate(ylat_r4)
    if (allocated(xlon_crs)) deallocate(xlon_crs)
    if (allocated(ylat_crs)) deallocate(ylat_crs)

    call nemsio_open(nemsiofile,trim(data_out),'write',rc,           &
      modelname="FV3GFS", gdatatype="bin4",                            &
      idate=idate,nfhour=int(fhr), nfminute=0,                         &
      nfsecondn=0, nfsecondd=1, nfday=0,                               &
      dimx=im,dimy=jm,dimz=mlev, nmeta=nmeta,idrt=idrt,                &
      nsoil=nsoil,ntrac=ntrac,nrec=nrec, ncldt=ncld,                   &
      idsl=idsl,idvc=idvc, idvm=idvm,                                  &
      vcoord=vcoord, lon=lon1d,lat=lat1d,                              &
      extrameta=.true.,recname=recname,                                &
      reclevtyp=reclevtyp,reclev=reclev,                               &
      nmetavari=nmetavari, variname=variname, varival=varival,         &
      nmetavarr=nmetavarr, nmetavarc=nmetavarc, varcname=varcname,     &
      varcval=varcval,nmetaaryi=nmetaaryi)
    if(rc/=0) print *,'nemsio_open, file=',trim(data_out),' iret=',rc


    !call nemsio_getrechead(nemsiofile,1,nvname,nvlevtyp,nvlev,rc)
    !print *,'nvname=', nvname

    nf=0
    do itf = 1, size(data_file)
       if (data_file(itf) /= '') nf=nf+1
    enddo

    do itf = 1, nf
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
          elseif (varname(iv)=='u') then
             ignore(iv)=.true.
             do_c2l(iv)=.false.
             cs_static(iv)=.true.
             print*, "WARNING: will ignore variable ",trim(varname(iv))
          elseif (varname(iv)=='v') then
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
          !if (time_type == nf90_double .or. time_type == nf90_float) then
          !  status = nf90_put_var(ncid_out, time_id, time(it))
          !else
          !  print*," time variable ",trim(time_name), " has to be real"
          !  stop
          !endif
          print *,'time =', time(it)
          do iv=1,nvars
             if (iv==u_id) then
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
    
                   if (finer_steps==0) then
                      allocate(arrayr4(nlon,nlat),tmp(nlon*nlat))
call timing_on('interp_res_wr')
                      if ( write_nemsioflip ) then
                        k1=nlev(iv); k2=1; k3=-1
                      else
                        k1=1; k2=nlev(iv); k3=1
                      endif
                      do j = 1, nvar3d
                         if (mdvarname(j) == varname(u_id)) then
                            jrec = (j - 1) * nlev(iv) + 1
                            exit
                         end if
                      end do
                      do k=k1,k2,k3
                        if ( write_nemsioflip ) then
                           do j=1,nlat
                              arrayr4(:,j) = ua_latlon(:,nlat-j+1,k)
                           end do
                        else
                           arrayr4 = ua_latlon(:,:,k)
                        end if
                        tmp = reshape(arrayr4, (/nlon*nlat/))
                        !print *,'ua, jrec', jrec
                        !print *,'in write nemsio, value=',maxval(arrayr4(1:nlon,1:nlat)), &
                        !minval(arrayr4(1:nlon,1:nlat)),maxloc(arrayr4(1:nlon,1:nlat)),minloc(arrayr4(1:nlon,1:nlat))
                        !call nemsio_getrechead(nemsiofile,jrec,nvname,nvlevtyp,nvlev,rc)
                        call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
                        !print *,'in write nemsio,fb=',i,' write jrec=',jrec,'fld is',  &
                        !         trim(nvname), 'rc=', &
                        !         rc, 'value=',maxval(tmp),minval(tmp),maxloc(tmp),minloc(tmp)
                        jrec = jrec + 1
                      enddo
call timing_off('interp_res_wr')
                      deallocate(arrayr4,tmp)
                   else
                      allocate(var_latlon_crs(nlon_crs,nlat_crs,nlev(iv)))
                      call do_latlon_coarsening(ua_latlon, da%ylat, nlon, nlat, nlev(iv),        &
                                                var_latlon_crs, nlon_crs, nlat_crs, finer_steps, &
                                                misval(u_id), varmisval(u_id))
   
                      allocate(arrayr4(nlon_crs,nlat_crs),tmp(nlon_crs*nlat_crs))
call timing_on('interp_res_wr')
                      if ( write_nemsioflip ) then
                        k1=nlev(iv); k2=1; k3=-1
                      else
                        k1=1; k2=nlev(iv); k3=1
                      endif
                      do j = 1, nvar3d
                        if (mdvarname(j) == varname(u_id)) then
                            jrec = (j - 1) * nlev(iv) + 1
                            exit
                         end if
                      end do
                      do k=k1,k2,k3
                        if ( write_nemsioflip ) then
                           do j=1,nlat_crs
                              arrayr4(:,j) = var_latlon_crs(:,nlat_crs-j+1,k)
                           end do
                        else
                           arrayr4 = var_latlon_crs(:,:,k)
                        end if
                        tmp = reshape(arrayr4, (/nlon_crs*nlat_crs/))
                        call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
                        jrec = jrec + 1
                      enddo
call timing_off('interp_res_wr')
                      deallocate(arrayr4,tmp)
                      deallocate(var_latlon_crs)
                   endif
                   !------------------------------------------------------!
                   ! calculate and write meridional latlon flow           !
                   !------------------------------------------------------!
                   if (finer_steps==0) then
                      allocate(arrayr4(nlon,nlat),tmp(nlon*nlat))
call timing_on('interp_res_wr')
                      if ( write_nemsioflip ) then
                        k1=nlev(iv); k2=1; k3=-1
                      else
                        k1=1; k2=nlev(iv); k3=1
                      endif
                      do j = 1, nvar3d
                         if (mdvarname(j) == varname(v_id)) then
                            jrec = (j - 1) * nlev(iv) + 1
                            exit
                         end if
                      end do
                      do k=k1,k2,k3
                        if ( write_nemsioflip ) then
                           do j=1,nlat
                              arrayr4(:,j) = va_latlon(:,nlat-j+1,k)
                           end do
                        else
                           arrayr4 = va_latlon(:,:,k)
                        end if
                        tmp = reshape(arrayr4, (/nlon*nlat/))
                        !print *,'va, jrec', jrec
                        !print *,'in write nemsio, value=',maxval(arrayr4(1:nlon,1:nlat)), &
                        !minval(arrayr4(1:nlon,1:nlat)),maxloc(arrayr4(1:nlon,1:nlat)),minloc(arrayr4(1:nlon,1:nlat))
                        !call nemsio_getrechead(nemsiofile,jrec,nvname,nvlevtyp,nvlev,rc)
                        call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
                        !print *,'in write nemsio,fb=',i,' write jrec=',jrec,'fld is',  &
                        !         trim(nvname), 'rc=', &
                        !         rc, 'value=',maxval(tmp),minval(tmp),maxloc(tmp),minloc(tmp)
                        jrec = jrec + 1
                      enddo
call timing_off('interp_res_wr')
                      deallocate(arrayr4,tmp)
                   else
                      allocate(var_latlon_crs(nlon_crs,nlat_crs,nlev(iv)))
                      call do_latlon_coarsening(va_latlon, da%ylat, nlon, nlat, nlev(iv),        &
                                                var_latlon_crs, nlon_crs, nlat_crs, finer_steps, &
                                                misval(v_id), varmisval(v_id))
                      allocate(arrayr4(nlon_crs,nlat_crs),tmp(nlon_crs*nlat_crs))
call timing_on('interp_res_wr')
                      if ( write_nemsioflip ) then
                        k1=nlev(iv); k2=1; k3=-1
                      else
                        k1=1; k2=nlev(iv); k3=1
                      endif
                      do j = 1, nvar3d
                        if (mdvarname(j) == varname(v_id)) then
                            jrec = (j - 1) * nlev(iv) + 1
                            exit
                         end if
                      end do
                      do k=k1,k2,k3
                        if ( write_nemsioflip ) then
                           do j=1,nlat_crs
                              arrayr4(:,j) = var_latlon_crs(:,nlat_crs-j+1,k)
                           end do
                        else
                           arrayr4 = var_latlon_crs(:,:,k)
                        end if
                        tmp = reshape(arrayr4, (/nlon_crs*nlat_crs/))
                        call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
                        jrec = jrec + 1
                      enddo
call timing_off('interp_res_wr')
                      deallocate(arrayr4,tmp)
                      deallocate(var_latlon_crs)
                   endif
                   deallocate(ua_latlon, va_latlon)
                endif
             elseif (iv/=v_id .and. (any(mdvarname(1:nvar3d) == varname(iv)) .or. &
                     varname(iv) == mdvarname(nmdvar))) then
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
                      elseif (varndims(iv)==4) then
                         start(1:4)=(/1,1,1,it/)
                         count(1:4)=(/npx-1,npy-1,nlev(iv),1/)
                         allocate(var_cubsph(0:npx,0:npy,nlev(iv),ntiles))
                         call init_corners(var_cubsph, nlev(iv))
                         allocate(var_latlon(nlon,nlat,nlev(iv)))
                         call nc_get_cube_variable(ncid_in, iv, vartype(iv), varoffset(iv), varscale(iv), &
                                                   varndims(iv), npx, npy, nlev(iv), ntiles, start, count, &
                                                   var_cubsph)
                      else
                         print*," unexpected number of dimensions (", varndims(iv), &
                                ") for variable ",trim(varname(iv))
                         stop
                      endif
call timing_off('interp_res_rd')
                      
                      call fv3_da_out_interp_scalar( &
                          da,var_cubsph,var_latlon,nlev(iv), &
                          misval(iv),varmisval(iv),fill_missing)
                   
                      if (finer_steps==0) then
                         allocate(arrayr4(nlon,nlat),tmp(nlon*nlat))
                         if (varndims(iv)==3 .and. varname(iv) == 'phis' ) then
call timing_on('interp_res_wr')
                            jrec = nvar3d * nlev(u_id) + 2
                            if ( write_nemsioflip ) then
                               do j=1,nlat
                                  arrayr4(:,j) = var_latlon(:,nlat-j+1,1)
                               end do
                            else
                               arrayr4 = var_latlon(:,:,1)
                            end if
                            tmp = reshape(arrayr4, (/nlon*nlat/))
                            tmp = tmp/grav 
                            !print *,'in write nemsio, phis, jrec=', jrec, maxval(arrayr4(1:nlon,1:nlat)), &
                            !minval(arrayr4(1:nlon,1:nlat)),maxloc(arrayr4(1:nlon,1:nlat)),minloc(arrayr4(1:nlon,1:nlat))
                            call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
call timing_off('interp_res_wr')
                         else
call timing_on('interp_res_wr')
                            if (varname(iv) == 'delp') then
                               allocate(psr4(nlon,nlat))
                               psr4 = ptop
                            end if
                            if ( write_nemsioflip ) then
                              k1=nlev(iv); k2=1; k3=-1
                            else
                              k1=1; k2=nlev(iv); k3=1
                            endif
                            do j = 1, nvar3d
                               if (mdvarname(j) == varname(iv)) then
                                  jrec = (j - 1) * nlev(iv) + 1
                                  exit
                               end if
                            end do
                            do k=k1,k2,k3
                              if ( write_nemsioflip ) then
                                 do j=1,nlat
                                   arrayr4(:,j) = var_latlon(:,nlat-j+1,k)
                                 end do
                              else
                                 arrayr4 = var_latlon(:,:,k)
                              end if
                           
                              if (varname(iv) == 'delp') then
                                 if ( write_nemsioflip ) then
                                    do j=1,nlat
                                       psr4(:,j) = psr4(:,j) + var_latlon(:,nlat-j+1,k)
                                    end do
                                 else
                                    psr4(:,:) = psr4(:,:) + var_latlon(:,:,k)
                                 end if
                              end if

                              tmp = reshape(arrayr4, (/nlon*nlat/))
                              !print *,'in write nemsio =', varname(iv), jrec, maxval(arrayr4(1:nlon,1:nlat)), &
                              ! minval(arrayr4(1:nlon,1:nlat)),maxloc(arrayr4(1:nlon,1:nlat)),minloc(arrayr4(1:nlon,1:nlat))
                              call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
                              jrec = jrec + 1
                            end do
                            if (varname(iv) == 'delp') then
                               tmp = reshape(psr4, (/nlon*nlat/))
                               jrec = nvar3d * nlev(iv) + 1
                               !print *,'in write nemsio =', varname(iv), jrec, maxval(arrayr4(1:nlon,1:nlat)), &
                               !minval(arrayr4(1:nlon,1:nlat)),maxloc(arrayr4(1:nlon,1:nlat)),minloc(arrayr4(1:nlon,1:nlat))
                               call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
                               deallocate(psr4)
                            end if
call timing_off('interp_res_wr')
                         endif
                         deallocate(arrayr4,tmp)
                      else
                         allocate(var_latlon_crs(nlon_crs,nlat_crs,nlev(iv)))
                         call do_latlon_coarsening(var_latlon, da%ylat, nlon, nlat, nlev(iv),  &
                              var_latlon_crs, nlon_crs, nlat_crs, finer_steps, &
                              misval(iv), varmisval(iv))
                         allocate(arrayr4(nlon,nlat),tmp(nlon*nlat))
                         if (varndims(iv)==3 .and. varname(iv) == 'phis') then
call timing_on('interp_res_wr') 
                            jrec = nvar3d * nlev(u_id) + 2
                            if ( write_nemsioflip ) then
                               do j=1,nlat_crs
                                  arrayr4(:,j) = var_latlon_crs(:,nlat_crs-j+1,1)
                               end do
                            else
                               arrayr4 = var_latlon_crs(:,:,1)
                            end if
                            tmp = reshape(arrayr4, (/nlon_crs*nlat_crs/))
                            tmp = tmp/grav
                            call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
call timing_off('interp_res_wr')
                         else
call timing_on('interp_res_wr')
                            if (varname(iv) == 'delp') then
                               allocate(psr4(nlon_crs,nlat_crs))
                               psr4 = 0.
                            end if
                            if ( write_nemsioflip ) then
                               k1=nlev(iv); k2=1; k3=-1
                            else
                               k1=1; k2=nlev(iv); k3=1
                            endif
                            do j = 1, nvar3d
                              if (mdvarname(j) == varname(iv)) then
                                  jrec = (j - 1) * nlev(iv) + 1
                                  exit
                               end if
                            end do
                            do k=k1,k2,k3
                              if ( write_nemsioflip ) then
                                 do j=1,nlat_crs
                                    arrayr4(:,j) = var_latlon_crs(:,nlat_crs-j+1,k)
                                 end do
                              else
                                 arrayr4 = var_latlon_crs(:,:,k)
                              end if
                              if (varname(iv) == 'delp') then
                                 do j=1,nlat_crs
                                    psr4(:,j) = psr4(:,j) + var_latlon_crs(:,nlat_crs-j+1,k)
                                 end do
                              else
                                 psr4(:,:) = psr4(:,:) + var_latlon_crs(:,:,k)
                              end if
                              tmp = reshape(arrayr4, (/nlon_crs*nlat_crs/))
                              call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
                              jrec = jrec + 1
                            enddo
                            if (varname(iv) == 'delp') then
                               tmp = reshape(psr4, (/nlon*nlat/))
                               jrec = nvar3d * nlev(iv) + 1
                               call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
                               deallocate(psr4)
                            end if
call timing_off('interp_res_wr')
                         endif
                         deallocate(var_latlon_crs)
                         deallocate(arrayr4,tmp)
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

    call nemsio_close(nemsiofile, iret=rc)

    call nemsio_finalize()

    contains
      subroutine init_corners(var, nz)
        integer, intent(in) :: nz
        real, dimension(0:npx,0:npy,nz,ntiles), intent(inout) :: var

        var(  0,  0,:,:)=0.
        var(  0,npy,:,:)=0.
        var(npx,npy,:,:)=0.
        var(npx,  0,:,:)=0.
      end subroutine init_corners
  end subroutine interp_res_nemsio

end module interp_res_nemsio_mod



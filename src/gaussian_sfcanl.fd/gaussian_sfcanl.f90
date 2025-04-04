!------------------------------------------------------------------
!
! Read in surface and nst data on the cubed-sphere grid,
! interpolate it to the gaussian grid, and output the result
! to a netcdf file.  To not process nst data,
! set flag 'donst' to '.false.'.  To process nst, set to '.true.'.
!
! Input files:
! ------------
! weights.nc              Interpolation weights.  netcdf format
! anal.tile[1-6].nc       fv3 surface restart files
! orog.tile[1-6].nc       fv3 orography files
! fort.41 namelist        Configuration namelist
! vcoord.txt              Vertical coordinate definition file
!                         (ascii)
!
! Output files:
! -------------
! sfc.gaussian.analysis.file  surface data on gaussian grid - netcdf.
!
! Namelist variables:
! -------------------
! yy/mm/dd/hh             year/month/day/hour of data.
! i/jgaus                 i/j dimension of gaussian grid.
! donst                   When '.true.' process nst data, default; .false.
!
! 2018-Jan-30 Gayno       Initial version
! 2019-Oct-30 Gayno       Option to output gaussian analysis file
!                         in netcdf.
! 2023-Mar-04 Mahajan     Deprecate option to output gaussian analysis file
!                         in nemsio.
!
!------------------------------------------------------------------

 module io

 implicit none

 logical :: donst

 integer, parameter :: num_tiles = 6

 integer :: itile, jtile, igaus, jgaus
 real :: fhr

 integer :: idate(8)

 type :: sfc_data
! surface variables
   real, allocatable :: alvsf(:)
   real, allocatable :: alvwf(:)
   real, allocatable :: alnsf(:)
   real, allocatable :: alnwf(:)
   real, allocatable :: canopy(:)
   real, allocatable :: facsf(:)
   real, allocatable :: facwf(:)
   real, allocatable :: ffhh(:)
   real, allocatable :: ffmm(:)
   real, allocatable :: fice(:)
   real, allocatable :: f10m(:)
   real, allocatable :: hice(:)
   real, allocatable :: q2m(:)
   real, allocatable :: orog(:)
   real, allocatable :: sheleg(:)
   real, allocatable :: slmask(:)
   real, allocatable :: shdmax(:)
   real, allocatable :: shdmin(:)
   real, allocatable :: slope(:)
   real, allocatable :: srflag(:)
   real, allocatable :: snoalb(:)
   real, allocatable :: snwdph(:)
   real, allocatable :: stype(:)
   real, allocatable :: t2m(:)
   real, allocatable :: tprcp(:)
   real, allocatable :: tisfc(:)
   real, allocatable :: tsea(:)
   real, allocatable :: tg3(:)
   real, allocatable :: uustar(:)
   real, allocatable :: vfrac(:)
   real, allocatable :: vtype(:)
   real, allocatable :: zorl(:)
   real, allocatable :: slc(:,:)
   real, allocatable :: smc(:,:)
   real, allocatable :: stc(:,:)
! nst variables
   real, allocatable :: c0(:)
   real, allocatable :: cd(:)
   real, allocatable :: dconv(:)
   real, allocatable :: dtcool(:)
   real, allocatable :: land(:)
   real, allocatable :: qrain(:)
   real, allocatable :: tref(:)
   real, allocatable :: w0(:)
   real, allocatable :: wd(:)
   real, allocatable :: xs(:)
   real, allocatable :: xt(:)
   real, allocatable :: xtts(:)
   real, allocatable :: xu(:)
   real, allocatable :: xv(:)
   real, allocatable :: xz(:)
   real, allocatable :: xzts(:)
   real, allocatable :: zc(:)
 end type sfc_data
 
 type(sfc_data) :: tile_data, gaussian_data

 end module io

!------------------------------------------------------------------------------
! Main program 
!------------------------------------------------------------------------------

 program main

 use netcdf
 use io

 implicit none

 character(len=12)       :: weightfile

 integer                 :: i, error, ncid, id_ns, n_s, n_s2
 integer                 :: id_col, id_row, id_s, n
 integer                 :: yy, mm, dd, hh
 integer, allocatable    :: col(:), row(:), col2(:), row2(:)

 real(kind=8), allocatable :: s(:), s2(:)

 namelist /setup/ yy, mm, dd, hh, fhr, igaus, jgaus, donst

 call w3tagb('GAUSSIAN_SFCANL',2018,0179,0055,'NP20')

 print*,"- BEGIN EXECUTION"

 fhr = 0.
 donst = .false.

 print*
 print*,"- READ SETUP NAMELIST"
 open(41, file="./fort.41")
 read(41, nml=setup, iostat=error)
 if (error /= 0) then
   print*,"** FATAL ERROR READING NAMELIST. ISTAT IS: ", error
   call errexit(56)
 endif
 close (41)

 idate = 0
 idate(1) = yy
 idate(2) = mm
 idate(3) = dd
 idate(4) = hh

!------------------------------------------------------------------------------
! Read interpolation weight file (Nearest).
!------------------------------------------------------------------------------

 print*
 print*,"- READ INTERPOLATION WEIGHT FILE"

 weightfile = "./weights.nc"

 error=nf90_open(trim(weightfile),nf90_nowrite,ncid)
 call netcdf_err(error, 'OPENING weights.nc' )

 error=nf90_inq_dimid(ncid, 'n_s', id_ns)
 call netcdf_err(error, 'READING n_s id' )
 error=nf90_inquire_dimension(ncid,id_ns,len=n_s)
 call netcdf_err(error, 'READING n_s' )

 allocate(col(n_s))
 error=nf90_inq_varid(ncid, 'col', id_col)
 call netcdf_err(error, 'READING col id' )
 error=nf90_get_var(ncid, id_col, col)
 call netcdf_err(error, 'READING col' )

 allocate(row(n_s))
 error=nf90_inq_varid(ncid, 'row', id_row)
 call netcdf_err(error, 'READING row id' )
 error=nf90_get_var(ncid, id_row, row)
 call netcdf_err(error, 'READING row' )

 allocate(s(n_s))
 error=nf90_inq_varid(ncid, 'S', id_s)
 call netcdf_err(error, 'READING s id' )
 error=nf90_get_var(ncid, id_s, s)
 call netcdf_err(error, 'READING s' )

 error = nf90_close(ncid)

!------------------------------------------------------------------------------
! Read interpolation weight file (Bilinear).
!------------------------------------------------------------------------------

 print*
 print*,"- READ INTERPOLATION WEIGHT FILE"

 weightfile = "./weightb.nc"

 error=nf90_open(trim(weightfile),nf90_nowrite,ncid)
 call netcdf_err(error, 'OPENING weights.nc' )

 error=nf90_inq_dimid(ncid, 'n_s', id_ns)
 call netcdf_err(error, 'READING n_s id' )
 error=nf90_inquire_dimension(ncid,id_ns,len=n_s2)
 call netcdf_err(error, 'READING n_s' )

 allocate(col2(n_s2))
 error=nf90_inq_varid(ncid, 'col', id_col)
 call netcdf_err(error, 'READING col id' )
 error=nf90_get_var(ncid, id_col, col2)
 call netcdf_err(error, 'READING col' )

 allocate(row2(n_s2))
 error=nf90_inq_varid(ncid, 'row', id_row)
 call netcdf_err(error, 'READING row id' )
 error=nf90_get_var(ncid, id_row, row2)
 call netcdf_err(error, 'READING row' )

 allocate(s2(n_s2))
 error=nf90_inq_varid(ncid, 'S', id_s)
 call netcdf_err(error, 'READING s id' )
 error=nf90_get_var(ncid, id_s, s2)
 call netcdf_err(error, 'READING s' )

 error = nf90_close(ncid)

!------------------------------------------------------------------------------
! Read the tiled analysis data.
!------------------------------------------------------------------------------

 call read_data_anl

!------------------------------------------------------------------------------
! Interpolate tiled data to gaussian grid.
!------------------------------------------------------------------------------

 allocate(gaussian_data%orog(igaus*jgaus))    ! sfc
 allocate(gaussian_data%t2m(igaus*jgaus))
 allocate(gaussian_data%tisfc(igaus*jgaus))
 allocate(gaussian_data%q2m(igaus*jgaus))
 allocate(gaussian_data%stype(igaus*jgaus))
 allocate(gaussian_data%snwdph(igaus*jgaus))
 allocate(gaussian_data%slope(igaus*jgaus))
 allocate(gaussian_data%shdmax(igaus*jgaus))
 allocate(gaussian_data%shdmin(igaus*jgaus))
 allocate(gaussian_data%snoalb(igaus*jgaus))
 allocate(gaussian_data%slmask(igaus*jgaus))
 allocate(gaussian_data%tg3(igaus*jgaus))
 allocate(gaussian_data%alvsf(igaus*jgaus))
 allocate(gaussian_data%alvwf(igaus*jgaus))
 allocate(gaussian_data%alnsf(igaus*jgaus))
 allocate(gaussian_data%alnwf(igaus*jgaus))
 allocate(gaussian_data%facsf(igaus*jgaus))
 allocate(gaussian_data%facwf(igaus*jgaus))
 allocate(gaussian_data%ffhh(igaus*jgaus))
 allocate(gaussian_data%ffmm(igaus*jgaus))
 allocate(gaussian_data%sheleg(igaus*jgaus))
 allocate(gaussian_data%canopy(igaus*jgaus))
 allocate(gaussian_data%vfrac(igaus*jgaus))
 allocate(gaussian_data%vtype(igaus*jgaus))
 allocate(gaussian_data%zorl(igaus*jgaus))
 allocate(gaussian_data%tsea(igaus*jgaus))
 allocate(gaussian_data%f10m(igaus*jgaus))
 allocate(gaussian_data%tprcp(igaus*jgaus))
 allocate(gaussian_data%uustar(igaus*jgaus))
 allocate(gaussian_data%fice(igaus*jgaus))
 allocate(gaussian_data%hice(igaus*jgaus))
 allocate(gaussian_data%srflag(igaus*jgaus))
 allocate(gaussian_data%slc(igaus*jgaus,4))
 allocate(gaussian_data%smc(igaus*jgaus,4))
 allocate(gaussian_data%stc(igaus*jgaus,4))

 if (donst) then
   allocate(gaussian_data%c0(igaus*jgaus))  ! nst
   allocate(gaussian_data%cd(igaus*jgaus))  
   allocate(gaussian_data%dconv(igaus*jgaus))  
   allocate(gaussian_data%dtcool(igaus*jgaus)) 
   allocate(gaussian_data%land(igaus*jgaus)) 
   allocate(gaussian_data%qrain(igaus*jgaus)) 
   allocate(gaussian_data%tref(igaus*jgaus)) 
   allocate(gaussian_data%w0(igaus*jgaus)) 
   allocate(gaussian_data%wd(igaus*jgaus)) 
   allocate(gaussian_data%xs(igaus*jgaus)) 
   allocate(gaussian_data%xt(igaus*jgaus)) 
   allocate(gaussian_data%xtts(igaus*jgaus)) 
   allocate(gaussian_data%xu(igaus*jgaus)) 
   allocate(gaussian_data%xv(igaus*jgaus)) 
   allocate(gaussian_data%xz(igaus*jgaus)) 
   allocate(gaussian_data%xzts(igaus*jgaus)) 
   allocate(gaussian_data%zc(igaus*jgaus)) 
 endif

 gaussian_data%orog=0.0 
 gaussian_data%t2m=0.0
 gaussian_data%tisfc=0.0
 gaussian_data%q2m=0.0
 gaussian_data%stype=0.0
 gaussian_data%snwdph=0.0
 gaussian_data%slope=0.0
 gaussian_data%shdmax=0.0
 gaussian_data%shdmin=0.0
 gaussian_data%snoalb=0.0
 gaussian_data%slmask=0.0
 gaussian_data%tg3=0.0
 gaussian_data%alvsf=0.0
 gaussian_data%alvwf=0.0
 gaussian_data%alnsf=0.0
 gaussian_data%alnwf=0.0
 gaussian_data%facsf=0.0
 gaussian_data%facwf=0.0
 gaussian_data%ffhh=0.0
 gaussian_data%ffmm=0.0
 gaussian_data%sheleg=0.0
 gaussian_data%canopy=0.0
 gaussian_data%vfrac=0.0
 gaussian_data%vtype=0.0
 gaussian_data%zorl=0.0
 gaussian_data%tsea=0.0
 gaussian_data%f10m=0.0
 gaussian_data%tprcp=0.0
 gaussian_data%uustar=0.0
 gaussian_data%fice=0.0
 gaussian_data%hice=0.0
 gaussian_data%srflag=0.0
 gaussian_data%slc=0.0
 gaussian_data%smc=0.0
 gaussian_data%stc=0.0

 do i = 1, n_s2
!   gaussian_data%orog(row2(i))   = gaussian_data%orog(row2(i)) + s2(i)*tile_data%orog(col2(i))
   gaussian_data%t2m(row2(i))    = gaussian_data%t2m(row2(i)) + s2(i)*tile_data%t2m(col2(i))
   gaussian_data%q2m(row2(i))    = gaussian_data%q2m(row2(i)) + s2(i)*tile_data%q2m(col2(i))
 enddo
 
 do i = 1, n_s
   gaussian_data%orog(row(i))   = gaussian_data%orog(row(i)) + s(i)*tile_data%orog(col(i))
!   gaussian_data%t2m(row(i))    = gaussian_data%t2m(row(i)) + s(i)*tile_data%t2m(col(i))
   gaussian_data%tisfc(row(i))  = gaussian_data%tisfc(row(i)) + s(i)*tile_data%tisfc(col(i))
!   gaussian_data%q2m(row(i))    = gaussian_data%q2m(row(i)) + s(i)*tile_data%q2m(col(i))
   gaussian_data%stype(row(i))  = gaussian_data%stype(row(i)) + s(i)*tile_data%stype(col(i))
   gaussian_data%snwdph(row(i)) = gaussian_data%snwdph(row(i)) + s(i)*tile_data%snwdph(col(i))
   gaussian_data%slope(row(i))  = gaussian_data%slope(row(i)) + s(i)*tile_data%slope(col(i))
   gaussian_data%shdmax(row(i)) = gaussian_data%shdmax(row(i)) + s(i)*tile_data%shdmax(col(i))
   gaussian_data%shdmin(row(i)) = gaussian_data%shdmin(row(i)) + s(i)*tile_data%shdmin(col(i))
   gaussian_data%slmask(row(i)) = gaussian_data%slmask(row(i)) + s(i)*tile_data%slmask(col(i))
   gaussian_data%tg3(row(i))    = gaussian_data%tg3(row(i)) + s(i)*tile_data%tg3(col(i))
   gaussian_data%alvsf(row(i))  = gaussian_data%alvsf(row(i)) + s(i)*tile_data%alvsf(col(i))
   gaussian_data%alvwf(row(i))  = gaussian_data%alvwf(row(i)) + s(i)*tile_data%alvwf(col(i))
   gaussian_data%alnsf(row(i))  = gaussian_data%alnsf(row(i)) + s(i)*tile_data%alnsf(col(i))
   gaussian_data%alnwf(row(i))  = gaussian_data%alnwf(row(i)) + s(i)*tile_data%alnwf(col(i))
   gaussian_data%sheleg(row(i)) = gaussian_data%sheleg(row(i)) + s(i)*tile_data%sheleg(col(i))
   gaussian_data%canopy(row(i)) = gaussian_data%canopy(row(i)) + s(i)*tile_data%canopy(col(i))
   gaussian_data%vfrac(row(i))  = gaussian_data%vfrac(row(i)) + s(i)*tile_data%vfrac(col(i))
   gaussian_data%zorl(row(i))   = gaussian_data%zorl(row(i)) + s(i)*tile_data%zorl(col(i))
   gaussian_data%tsea(row(i))   = gaussian_data%tsea(row(i)) + s(i)*tile_data%tsea(col(i))
   gaussian_data%f10m(row(i))   = gaussian_data%f10m(row(i)) + s(i)*tile_data%f10m(col(i))
   gaussian_data%vtype(row(i))  = gaussian_data%vtype(row(i)) + s(i)*tile_data%vtype(col(i))
   gaussian_data%tprcp(row(i))  = gaussian_data%tprcp(row(i)) + s(i)*tile_data%tprcp(col(i))
   gaussian_data%facsf(row(i))  = gaussian_data%facsf(row(i)) + s(i)*tile_data%facsf(col(i))
   gaussian_data%facwf(row(i))  = gaussian_data%facwf(row(i)) + s(i)*tile_data%facwf(col(i))
   gaussian_data%ffhh(row(i))   = gaussian_data%ffhh(row(i)) + s(i)*tile_data%ffhh(col(i))
   gaussian_data%ffmm(row(i))   = gaussian_data%ffmm(row(i)) + s(i)*tile_data%ffmm(col(i))
   gaussian_data%uustar(row(i)) = gaussian_data%uustar(row(i)) + s(i)*tile_data%uustar(col(i))
   gaussian_data%fice(row(i))   = gaussian_data%fice(row(i)) + s(i)*tile_data%fice(col(i))
   gaussian_data%hice(row(i))   = gaussian_data%hice(row(i)) + s(i)*tile_data%hice(col(i))
   gaussian_data%snoalb(row(i)) = gaussian_data%snoalb(row(i)) + s(i)*tile_data%snoalb(col(i))
   gaussian_data%srflag(row(i)) = gaussian_data%srflag(row(i)) + s(i)*tile_data%srflag(col(i))
   if (donst) then
     gaussian_data%c0(row(i))     = gaussian_data%c0(row(i)) + s(i)*tile_data%c0(col(i))
     gaussian_data%cd(row(i))     = gaussian_data%cd(row(i)) + s(i)*tile_data%cd(col(i))
     gaussian_data%dconv(row(i))  = gaussian_data%dconv(row(i)) + s(i)*tile_data%dconv(col(i))
     gaussian_data%dtcool(row(i)) = gaussian_data%dtcool(row(i)) + s(i)*tile_data%dtcool(col(i))
     gaussian_data%qrain(row(i))  = gaussian_data%qrain(row(i)) + s(i)*tile_data%qrain(col(i))
     gaussian_data%tref(row(i))   = gaussian_data%tref(row(i)) + s(i)*tile_data%tref(col(i))
     gaussian_data%w0(row(i))     = gaussian_data%w0(row(i)) + s(i)*tile_data%w0(col(i))
     gaussian_data%wd(row(i))     = gaussian_data%wd(row(i)) + s(i)*tile_data%wd(col(i))
     gaussian_data%xs(row(i))     = gaussian_data%xs(row(i)) + s(i)*tile_data%xs(col(i))
     gaussian_data%xt(row(i))     = gaussian_data%xt(row(i)) + s(i)*tile_data%xt(col(i))
     gaussian_data%xtts(row(i))   = gaussian_data%xtts(row(i)) + s(i)*tile_data%xtts(col(i))
     gaussian_data%xu(row(i))     = gaussian_data%xu(row(i)) + s(i)*tile_data%xu(col(i))
     gaussian_data%xv(row(i))     = gaussian_data%xv(row(i)) + s(i)*tile_data%xv(col(i))
     gaussian_data%xz(row(i))     = gaussian_data%xz(row(i)) + s(i)*tile_data%xz(col(i))
     gaussian_data%xzts(row(i))   = gaussian_data%xzts(row(i)) + s(i)*tile_data%xzts(col(i))
     gaussian_data%zc(row(i))     = gaussian_data%zc(row(i)) + s(i)*tile_data%zc(col(i))
   endif
   do n = 1, 4
     gaussian_data%slc(row(i),n) = gaussian_data%slc(row(i),n) + s(i)*tile_data%slc(col(i),n)
     gaussian_data%smc(row(i),n) = gaussian_data%smc(row(i),n) + s(i)*tile_data%smc(col(i),n)
     gaussian_data%stc(row(i),n) = gaussian_data%stc(row(i),n) + s(i)*tile_data%stc(col(i),n)
   enddo
 enddo

 deallocate(col, row, s)
 deallocate(col2, row2, s2)

 deallocate(tile_data%orog)
 deallocate(tile_data%t2m)
 deallocate(tile_data%tisfc)
 deallocate(tile_data%q2m)
 deallocate(tile_data%stype)
 deallocate(tile_data%snwdph)
 deallocate(tile_data%slope)
 deallocate(tile_data%shdmax)
 deallocate(tile_data%shdmin)
 deallocate(tile_data%snoalb)
 deallocate(tile_data%slmask)
 deallocate(tile_data%tg3)
 deallocate(tile_data%alvsf)
 deallocate(tile_data%alvwf)
 deallocate(tile_data%alnsf)
 deallocate(tile_data%alnwf)
 deallocate(tile_data%facsf)
 deallocate(tile_data%facwf)
 deallocate(tile_data%ffhh)
 deallocate(tile_data%ffmm)
 deallocate(tile_data%sheleg)
 deallocate(tile_data%canopy)
 deallocate(tile_data%vfrac)
 deallocate(tile_data%vtype)
 deallocate(tile_data%zorl)
 deallocate(tile_data%tsea)
 deallocate(tile_data%f10m)
 deallocate(tile_data%tprcp)
 deallocate(tile_data%uustar)
 deallocate(tile_data%fice)
 deallocate(tile_data%hice)
 deallocate(tile_data%srflag)
 deallocate(tile_data%slc)
 deallocate(tile_data%smc)
 deallocate(tile_data%stc)

 if (donst) then
   deallocate(tile_data%c0)
   deallocate(tile_data%cd)
   deallocate(tile_data%dconv)
   deallocate(tile_data%dtcool)
   deallocate(tile_data%qrain)
   deallocate(tile_data%tref)
   deallocate(tile_data%w0)
   deallocate(tile_data%wd)
   deallocate(tile_data%xs)
   deallocate(tile_data%xt)
   deallocate(tile_data%xtts)
   deallocate(tile_data%xu)
   deallocate(tile_data%xv)
   deallocate(tile_data%xz)
   deallocate(tile_data%xzts)
   deallocate(tile_data%zc)
 endif

!------------------------------------------------------------------------------
! Write gaussian data to netcdf file.
!------------------------------------------------------------------------------

 call write_sfc_data_netcdf

 deallocate(gaussian_data%orog)
 deallocate(gaussian_data%t2m)
 deallocate(gaussian_data%tisfc)
 deallocate(gaussian_data%q2m)
 deallocate(gaussian_data%stype)
 deallocate(gaussian_data%snwdph)
 deallocate(gaussian_data%slope)
 deallocate(gaussian_data%shdmax)
 deallocate(gaussian_data%shdmin)
 deallocate(gaussian_data%snoalb)
 deallocate(gaussian_data%slmask)
 deallocate(gaussian_data%tg3)
 deallocate(gaussian_data%alvsf)
 deallocate(gaussian_data%alvwf)
 deallocate(gaussian_data%alnsf)
 deallocate(gaussian_data%alnwf)
 deallocate(gaussian_data%facsf)
 deallocate(gaussian_data%facwf)
 deallocate(gaussian_data%ffhh)
 deallocate(gaussian_data%ffmm)
 deallocate(gaussian_data%sheleg)
 deallocate(gaussian_data%canopy)
 deallocate(gaussian_data%vfrac)
 deallocate(gaussian_data%vtype)
 deallocate(gaussian_data%zorl)
 deallocate(gaussian_data%tsea)
 deallocate(gaussian_data%f10m)
 deallocate(gaussian_data%tprcp)
 deallocate(gaussian_data%uustar)
 deallocate(gaussian_data%fice)
 deallocate(gaussian_data%hice)
 deallocate(gaussian_data%srflag)
 deallocate(gaussian_data%slc)
 deallocate(gaussian_data%smc)
 deallocate(gaussian_data%stc)

 if (donst) then
   deallocate(gaussian_data%c0)
   deallocate(gaussian_data%cd)
   deallocate(gaussian_data%dconv)
   deallocate(gaussian_data%dtcool)
   deallocate(gaussian_data%land)
   deallocate(gaussian_data%qrain)
   deallocate(gaussian_data%tref)
   deallocate(gaussian_data%w0)
   deallocate(gaussian_data%wd)
   deallocate(gaussian_data%xs)
   deallocate(gaussian_data%xt)
   deallocate(gaussian_data%xtts)
   deallocate(gaussian_data%xu)
   deallocate(gaussian_data%xv)
   deallocate(gaussian_data%xz)
   deallocate(gaussian_data%xzts)
   deallocate(gaussian_data%zc)
 endif

 print*
 print*,'- NORMAL TERMINATION'

 call w3tage('GAUSSIAN_SFCANL')

 end program main

!-------------------------------------------------------------------------------------------
! Write gaussian surface data to netcdf file.
!-------------------------------------------------------------------------------------------

 subroutine write_sfc_data_netcdf

 use netcdf
 use io

 implicit none

 character(len=50)       :: outfile
 character(len=31)       :: date_string
 character(len=4)        :: year
 character(len=2)        :: mon, day, hour

 integer                 :: header_buffer_val = 16384
 integer                 :: i, error, ncid, dim_xt, dim_yt, dim_time
 integer                 :: id_xt, id_yt, id_lon, id_lat, id_time
 integer                 :: n

! noah variables
 integer, parameter      :: num_noah=44
 character(len=30)       :: noah_var(num_noah)
 character(len=70)       :: noah_name(num_noah)
 character(len=30)       :: noah_units(num_noah)
 
! nst variables
 integer, parameter      :: num_nst=16
 character(len=30)       :: nst_var(num_nst)
 character(len=70)       :: nst_name(num_nst)
 character(len=30)       :: nst_units(num_nst)

! variables to be output
 integer                              :: num_vars
 character(len=30), allocatable       :: var(:)
 character(len=70), allocatable       :: name(:)
 character(len=30), allocatable       :: units(:)
 integer, allocatable                 :: id_var(:)

 real, parameter         :: missing = 9.99e20

 real(kind=4), allocatable :: dummy(:,:), slat(:), wlat(:)

! define noah fields

 data noah_var /"alnsf", &
                "alnwf", &
                "alvsf", &
                "alvwf", &
                "cnwat", &
                "crain",&
                "f10m", &
                "facsf", &
                "facwf", &
                "ffhh", &
                "ffmm", &
                "fricv", &
                "icec", &
                "icetk", &
                "land", &
                "orog", &
                "sfcr", &
                "shdmax", &
                "shdmin", &
                "sltyp", &
                "snoalb", &
                "snod", &
                "soill1", &
                "soill2", &
                "soill3", &
                "soill4", &
                "soilt1", &
                "soilt2", &
                "soilt3", &
                "soilt4", &
                "soilw1", &
                "soilw2", &
                "soilw3", &
                "soilw4", &
                "sotyp", &
                "spfh2m", &
                "tg3" , &
                "tisfc", &
                "tmp2m", &
                "tmpsfc", &
                "tprcp", &
                "veg", &
                "vtype", &
                "weasd"  /

 data noah_name /"mean nir albedo with strong cosz dependency", &
                 "mean nir albedo with weak cosz dependency", &
                 "mean vis albedo with strong cosz dependency", &
                 "mean vis albedo with weak cosz dependency", &
                 "canopy water (cnwat in gfs data)" , &
                 "instantaneous categorical rain", &
                 "10-meter wind speed divided by lowest model wind speed", &
                 "fractional coverage with strong cosz dependency", &
                 "fractional coverage with weak cosz dependency", &
                 "fh parameter from PBL scheme" , &
                 "fm parameter from PBL scheme" , &
                 "uustar surface frictional wind", &
                 "surface ice concentration (ice=1; no ice=0)", &
                 "sea ice thickness (icetk in gfs_data)", &
                 "sea-land-ice mask (0-sea, 1-land, 2-ice)", &
                 "surface geopotential height", &
                 "surface roughness", &
                 "maximum fractional coverage of green vegetation", &
                 "minimum fractional coverage of green vegetation", &
                 "surface slope type" , &
                 "maximum snow albedo in fraction", &
                 "surface snow depth", &
                 "liquid soil moisture at layer-1", &
                 "liquid soil moisture at layer-2", &
                 "liquid soil moisture at layer-3", &
                 "liquid soil moisture at layer-4", &
                 "soil temperature 0-10cm", &
                 "soil temperature 10-40cm", &
                 "soil temperature 40-100cm", &
                 "soil temperature 100-200cm", &
                 "volumetric soil moisture 0-10cm", &
                 "volumetric soil moisture 10-40cm", &
                 "volumetric soil moisture 40-100cm", &
                 "volumetric soil moisture 100-200cm", &
                 "soil type in integer", &
                 "2m specific humidity" , &
                 "deep soil temperature" , &
                 "surface temperature over ice fraction", &
                 "2m temperature", &
                 "surface temperature", &
                 "total precipitation" , &
                 "vegetation fraction", &
                 "vegetation type in integer", &
                 "surface snow water equivalent"  /

 data noah_units /"%", &
                  "%", &
                  "%", &
                  "%", &
                  "XXX", &
                  "number", &
                  "N/A", &
                  "XXX", &
                  "XXX", &
                  "XXX", &
                  "XXX", &
                  "XXX", &
                  "fraction", &
                  "XXX", &
                  "numerical", &
                  "gpm", &
                  "m", &
                  "XXX", &
                  "XXX", &
                  "XXX", &
                  "XXX", &
                  "m", &
                  "XXX", &
                  "XXX", &
                  "XXX", &
                  "XXX", &
                  "K", &
                  "K", &
                  "K", &
                  "K", &
                  "fraction", &
                  "fraction", &
                  "fraction", &
                  "fraction", &
                  "number", &
                  "kg/kg", & 
                  "K", &
                  "K", &
                  "K", &
                  "K", &
                  "kg/m**2", &
                  "fraction", &
                  "number" , &
                  "kg/m**2" /

! define nst fields

 data nst_var /"c0", &
               "cd", &
               "dconv", &
               "dtcool", &
               "qrain", &
               "tref", &
               "w0", &
               "wd", &
               "xs", &
               "xt", &
               "xtts", &
               "xu", &
               "xv", &
               "xz", &
               "xzts", &
               "zc" /

 data nst_name /"nsst coefficient1 to calculate d(tz)/d(ts)", &
                "nsst coefficient2 to calculate d(tz)/d(ts)", &
                "nsst thickness of free convection layer", &
                "nsst sub-layer cooling amount", &
                "nsst sensible heat flux due to rainfall", &
                "nsst reference or foundation temperature", &
                "nsst coefficient3 to calculate d(tz)/d(ts)", &
                "nsst coefficient4 to calculate d(tz)/d(ts)", &
                "nsst salinity content in diurnal thermocline layer", &
                "nsst heat content in diurnal thermocline layer", &
                "nsst d(xt)/d(ts)", &
                "nsst u-current content in diurnal thermocline layer", &
                "nsst v-current content in diurnal thermocline layer", &
                "nsst diurnal thermocline layer thickness", &
                "nsst d(xt)/d(ts)", &
                "nsst sub-layer cooling thickness"/

 data nst_units /"numerical", &
                 "n/a", &
                 "m", &
                 "k", &
                 "w/m2", &
                 "K", &
                 "n/a", &
                 "n/a", &
                 "n/a", &
                 "k*m", &
                 "m", &
                 "m2/s", &
                 "m2/s", &
                 "m", &
                 "m/k", &
                 "m"/

 outfile = "./sfc.gaussian.analysis.file"

 print*,"- WRITE SURFACE DATA TO NETCDF FILE: ", trim(outfile)

 error = nf90_create(outfile, cmode=IOR(IOR(NF90_CLOBBER,NF90_NETCDF4),NF90_CLASSIC_MODEL), ncid=ncid)
 call netcdf_err(error, 'CREATING NETCDF FILE')

! dimensions

 error = nf90_def_dim(ncid, 'grid_xt', igaus, dim_xt)
 call netcdf_err(error, 'DEFINING GRID_XT DIMENSION')

 error = nf90_def_dim(ncid, 'grid_yt', jgaus, dim_yt)
 call netcdf_err(error, 'DEFINING GRID_YT DIMENSION')

 error = nf90_def_dim(ncid, 'time', 1, dim_time)
 call netcdf_err(error, 'DEFINING TIME DIMENSION')

! global attributes

 error = nf90_put_att(ncid, nf90_global, 'ncld', 5)
 call netcdf_err(error, 'DEFINING NCLD ATTRIBUTE')

 error = nf90_put_att(ncid, nf90_global, 'nsoil', 4)
 call netcdf_err(error, 'DEFINING NSOIL ATTRIBUTE')

 error = nf90_put_att(ncid, nf90_global, 'source', "FV3GFS")
 call netcdf_err(error, 'DEFINING SOURCE ATTRIBUTE')

 error = nf90_put_att(ncid, nf90_global, 'grid', "gaussian")
 call netcdf_err(error, 'DEFINING GRID ATTRIBUTE')

 error = nf90_put_att(ncid, nf90_global, 'im', igaus)
 call netcdf_err(error, 'DEFINING IM ATTRIBUTE')

 error = nf90_put_att(ncid, nf90_global, 'jm', jgaus)
 call netcdf_err(error, 'DEFINING JM ATTRIBUTE')

! variables

! grid_xt

 error = nf90_def_var(ncid, 'grid_xt', NF90_DOUBLE, dim_xt, id_xt)
 call netcdf_err(error, 'DEFINING GRID_XT')

 error = nf90_put_att(ncid, id_xt, "cartesian_axis", "X")
 call netcdf_err(error, 'DEFINING GRID_XT ATTRIBUTE')

 error = nf90_put_att(ncid, id_xt, "long_name", "T-cell longitude")
 call netcdf_err(error, 'DEFINING GRID_XT ATTRIBUTE')

 error = nf90_put_att(ncid, id_xt, "units", "degrees_E")
 call netcdf_err(error, 'DEFINING GRID_XT ATTRIBUTE')

! lon

 error = nf90_def_var(ncid, 'lon', NF90_DOUBLE, (/dim_xt,dim_yt/), id_lon)
 call netcdf_err(error, 'DEFINING LON')

 error = nf90_put_att(ncid, id_lon, "long_name", "T-cell longitude")
 call netcdf_err(error, 'DEFINING LON ATTRIBUTE')

 error = nf90_put_att(ncid, id_lon, "units", "degrees_E")
 call netcdf_err(error, 'DEFINING LON ATTRIBUTE')

! grid_yt

 error = nf90_def_var(ncid, 'grid_yt', NF90_DOUBLE, dim_yt, id_yt)
 call netcdf_err(error, 'DEFINING GRID_YT')

 error = nf90_put_att(ncid, id_yt, "cartesian_axis", "Y")
 call netcdf_err(error, 'DEFINING GRID_YT ATTRIBUTE')

 error = nf90_put_att(ncid, id_yt, "long_name", "T-cell latitude")
 call netcdf_err(error, 'DEFINING GRID_YT ATTRIBUTE')

 error = nf90_put_att(ncid, id_yt, "units", "degrees_N")
 call netcdf_err(error, 'DEFINING GRID_YT ATTRIBUTE')

! lat

 error = nf90_def_var(ncid, 'lat', NF90_DOUBLE, (/dim_xt,dim_yt/), id_lat)
 call netcdf_err(error, 'DEFINING LAT')

 error = nf90_put_att(ncid, id_lat, "long_name", "T-cell latitude")
 call netcdf_err(error, 'DEFINING LAT ATTRIBUTE')

 error = nf90_put_att(ncid, id_lat, "units", "degrees_N")
 call netcdf_err(error, 'DEFINING LAT ATTRIBUTE')

! time

 error = nf90_def_var(ncid, 'time', NF90_DOUBLE, dim_time, id_time)
 call netcdf_err(error, 'DEFINING TIME')

 error = nf90_put_att(ncid, id_time, "long_name", "time")
 call netcdf_err(error, 'DEFINING TIME ATTRIBUTE')

 write(year, "(i4)") idate(1)
 write(mon, "(i2.2)") idate(2)
 write(day, "(i2.2)") idate(3)
 write(hour, "(i2.2)") idate(4)

 date_string="hours since " // year // "-" // mon // "-" // day // " " // hour // ":00:00"

 error = nf90_put_att(ncid, id_time, "units", date_string)
 call netcdf_err(error, 'DEFINING TIME ATTRIBUTE')

 error = nf90_put_att(ncid, id_time, "cartesian_axis", "T")
 call netcdf_err(error, 'DEFINING TIME ATTRIBUTE')

 error = nf90_put_att(ncid, id_time, "calendar_type", "JULIAN")
 call netcdf_err(error, 'DEFINING TIME ATTRIBUTE')

 error = nf90_put_att(ncid, id_time, "calendar", "JULIAN")
 call netcdf_err(error, 'DEFINING TIME ATTRIBUTE')

!-------------------------------------------------------------------------------------------
! Determine what variables to output (noah, or noah plus nst).
!-------------------------------------------------------------------------------------------
 
 if (donst) then
   num_vars = num_noah + num_nst
 else
   num_vars = num_noah
 endif
   
 allocate(var(num_vars))
 allocate(name(num_vars))
 allocate(units(num_vars))
 allocate(id_var(num_vars))

 var(1:num_noah) = noah_var
 name(1:num_noah) = noah_name
 units(1:num_noah) = noah_units

 if (donst) then
   do n = 1, num_nst
     var(n+num_noah) = nst_var(n)
     name(n+num_noah) = nst_name(n)
     units(n+num_noah) = nst_units(n)
   enddo
 endif

!-------------------------------------------------------------------------------------------
! Define variables in netcdf file.
!-------------------------------------------------------------------------------------------

 do n = 1, num_vars

   print*,'- DEFINE VARIABLE ',trim(var(n))
   error = nf90_def_var(ncid, trim(var(n)), NF90_FLOAT, (/dim_xt,dim_yt,dim_time/), id_var(n))
   call netcdf_err(error, 'DEFINING variable')
   error = nf90_def_var_deflate(ncid, id_var(n), 1, 1, 1)
   call netcdf_err(error, 'DEFINING variable with compression')

   error = nf90_put_att(ncid, id_var(n), "long_name", trim(name(n)))
   call netcdf_err(error, 'DEFINING name ATTRIBUTE')

   error = nf90_put_att(ncid, id_var(n), "units", trim(units(n)))
   call netcdf_err(error, 'DEFINING units ATTRIBUTE')

   error = nf90_put_att(ncid, id_var(n), "missing", missing)
   call netcdf_err(error, 'DEFINING missing ATTRIBUTE')

   error = nf90_put_att(ncid, id_var(n), "cell_methods", "time: point")
   call netcdf_err(error, 'DEFINING cell method ATTRIBUTE')

   error = nf90_put_att(ncid, id_var(n), "output_file", "sfc")
   call netcdf_err(error, 'DEFINING out file ATTRIBUTE')

 enddo

! end variable defs

 error = nf90_enddef(ncid, header_buffer_val,4,0,4)
 call netcdf_err(error, 'DEFINING HEADER')

!-------------------------------------------------------------------------------------------
! Write variables to netcdf file.
!-------------------------------------------------------------------------------------------

 allocate(dummy(igaus,jgaus))
 do i = 1, igaus
   dummy(i,:) = real((i-1),4) * 360.0_4 / real(igaus,4)
 enddo

 error = nf90_put_var(ncid, id_xt, dummy(:,1))
 call netcdf_err(error, 'WRITING GRID_XT')

 error = nf90_put_var(ncid, id_lon, dummy)
 call netcdf_err(error, 'WRITING LON')

 allocate(slat(jgaus))
 allocate(wlat(jgaus))
 call splat(4, jgaus, slat, wlat)

 do i = (jgaus/2+1), jgaus
   dummy(:,i) = 90.0 - (acos(slat(i)) * 180.0 / (4.0*atan(1.0)))
 enddo

 do i = 1, (jgaus/2)
   dummy(:,i) = -(dummy(:,(jgaus-i+1)))
 enddo

 deallocate(slat, wlat)

 error = nf90_put_var(ncid, id_yt, dummy(1,:))
 call netcdf_err(error, 'WRITING GRID_YT')

 error = nf90_put_var(ncid, id_lat, dummy)
 call netcdf_err(error, 'WRITING LAT')

 error = nf90_put_var(ncid, id_time, fhr)
 call netcdf_err(error, 'WRITING TIME')

 do n = 1, num_vars
   print*,'- WRITE VARIABLE ',trim(var(n))
   call get_netcdf_var(var(n), dummy)
   error = nf90_put_var(ncid, id_var(n), dummy, start=(/1,1,1/), count=(/igaus,jgaus,1/))
   call netcdf_err(error, 'WRITING variable')
 enddo

 deallocate (dummy)

 error = nf90_close(ncid)

 end subroutine write_sfc_data_netcdf

!-------------------------------------------------------------------------------------------
! Retrieve variable based on its netcdf identifier.
!-------------------------------------------------------------------------------------------

 subroutine get_netcdf_var(var, dummy)

 use io

 implicit none
 
 character(len=*), intent(in) :: var

 real(kind=4), intent(out) :: dummy(igaus,jgaus)

 select case (var)
   case ('alnsf')
     dummy = reshape(gaussian_data%alnsf, (/igaus,jgaus/))
   case ('alnwf')
     dummy = reshape(gaussian_data%alnwf, (/igaus,jgaus/))
   case ('alvsf')
     dummy = reshape(gaussian_data%alvsf, (/igaus,jgaus/))
   case ('alvwf')
     dummy = reshape(gaussian_data%alvwf, (/igaus,jgaus/))
   case ('cnwat')
     dummy = reshape(gaussian_data%canopy, (/igaus,jgaus/))
   case ('f10m')
     dummy = reshape(gaussian_data%f10m, (/igaus,jgaus/))
   case ('facsf')
     dummy = reshape(gaussian_data%facsf, (/igaus,jgaus/))
   case ('facwf')
     dummy = reshape(gaussian_data%facwf, (/igaus,jgaus/))
   case ('ffhh')
     dummy = reshape(gaussian_data%ffhh, (/igaus,jgaus/))
   case ('ffmm')
     dummy = reshape(gaussian_data%ffmm, (/igaus,jgaus/))
   case ('fricv')
     dummy = reshape(gaussian_data%uustar, (/igaus,jgaus/))
   case ('land')
     dummy = reshape(gaussian_data%slmask, (/igaus,jgaus/))
   case ('orog')
     dummy = reshape(gaussian_data%orog, (/igaus,jgaus/))
   case ('sltyp')
     dummy = reshape(gaussian_data%slope, (/igaus,jgaus/))
   case ('icec')
     dummy = reshape(gaussian_data%fice, (/igaus,jgaus/))
   case ('icetk')
     dummy = reshape(gaussian_data%hice, (/igaus,jgaus/))
   case ('snoalb')
     dummy = reshape(gaussian_data%snoalb, (/igaus,jgaus/))
   case ('shdmin')
     dummy = reshape(gaussian_data%shdmin, (/igaus,jgaus/))
   case ('shdmax')
     dummy = reshape(gaussian_data%shdmax, (/igaus,jgaus/))
   case ('snod')
     dummy = reshape(gaussian_data%snwdph, (/igaus,jgaus/)) / 1000.0
   case ('weasd')
     dummy = reshape(gaussian_data%sheleg, (/igaus,jgaus/))
   case ('veg')
     dummy = reshape(gaussian_data%vfrac, (/igaus,jgaus/)) * 100.0
   case ('sfcr')
     dummy = reshape(gaussian_data%zorl, (/igaus,jgaus/)) / 100.0
   case ('crain')
     dummy = reshape(gaussian_data%srflag, (/igaus,jgaus/))
   case ('sotyp')
     dummy = reshape(gaussian_data%stype, (/igaus,jgaus/))
   case ('spfh2m')
     dummy = reshape(gaussian_data%q2m, (/igaus,jgaus/))
   case ('tmp2m')
     dummy = reshape(gaussian_data%t2m, (/igaus,jgaus/))
   case ('tmpsfc')
     dummy = reshape(gaussian_data%tsea, (/igaus,jgaus/))
   case ('tg3')
     dummy = reshape(gaussian_data%tg3, (/igaus,jgaus/))
   case ('tisfc')
     dummy = reshape(gaussian_data%tisfc, (/igaus,jgaus/))
   case ('tprcp')
     dummy = reshape(gaussian_data%tprcp, (/igaus,jgaus/))
   case ('vtype')
     dummy = reshape(gaussian_data%vtype, (/igaus,jgaus/))
   case ('soill1')
     dummy = reshape(gaussian_data%slc(:,1), (/igaus,jgaus/))
     if(fhr == 0.) then
        where (dummy > 0.99) dummy = 0.0  ! replace flag value at water/landice
     endif
   case ('soill2')
     dummy = reshape(gaussian_data%slc(:,2), (/igaus,jgaus/))
     if(fhr == 0.) then
        where (dummy > 0.99) dummy = 0.0  ! replace flag value at water/landice
     endif
   case ('soill3')
     dummy = reshape(gaussian_data%slc(:,3), (/igaus,jgaus/))
     if(fhr == 0.) then
        where (dummy > 0.99) dummy = 0.0  ! replace flag value at water/landice
     endif
   case ('soill4')
     dummy = reshape(gaussian_data%slc(:,4), (/igaus,jgaus/))
     if(fhr == 0.) then
        where (dummy > 0.99) dummy = 0.0  ! replace flag value at water/landice
     endif
   case ('soilt1')
     dummy = reshape(gaussian_data%stc(:,1), (/igaus,jgaus/))
   case ('soilt2')
     dummy = reshape(gaussian_data%stc(:,2), (/igaus,jgaus/))
   case ('soilt3')
     dummy = reshape(gaussian_data%stc(:,3), (/igaus,jgaus/))
   case ('soilt4')
     dummy = reshape(gaussian_data%stc(:,4), (/igaus,jgaus/))
   case ('soilw1')
     dummy = reshape(gaussian_data%smc(:,1), (/igaus,jgaus/))
   case ('soilw2')
     dummy = reshape(gaussian_data%smc(:,2), (/igaus,jgaus/))
   case ('soilw3')
     dummy = reshape(gaussian_data%smc(:,3), (/igaus,jgaus/))
   case ('soilw4')
     dummy = reshape(gaussian_data%smc(:,4), (/igaus,jgaus/))
   case ('c0')
     dummy = reshape(gaussian_data%c0, (/igaus,jgaus/))
   case ('cd')
     dummy = reshape(gaussian_data%cd, (/igaus,jgaus/))
   case ('dconv')
     dummy = reshape(gaussian_data%dconv, (/igaus,jgaus/))
   case ('dtcool')
     dummy = reshape(gaussian_data%dtcool, (/igaus,jgaus/))
   case ('qrain')
     dummy = reshape(gaussian_data%qrain, (/igaus,jgaus/))
   case ('tref')
     dummy = reshape(gaussian_data%tref, (/igaus,jgaus/))
   case ('w0')
     dummy = reshape(gaussian_data%w0, (/igaus,jgaus/))
   case ('wd')
     dummy = reshape(gaussian_data%wd, (/igaus,jgaus/))
   case ('xs')
     dummy = reshape(gaussian_data%xs, (/igaus,jgaus/))
   case ('xt')
     dummy = reshape(gaussian_data%xt, (/igaus,jgaus/))
   case ('xtts')
     dummy = reshape(gaussian_data%xtts, (/igaus,jgaus/))
   case ('xu')
     dummy = reshape(gaussian_data%xu, (/igaus,jgaus/))
   case ('xv')
     dummy = reshape(gaussian_data%xv, (/igaus,jgaus/))
   case ('xz')
     dummy = reshape(gaussian_data%xz, (/igaus,jgaus/))
   case ('xzts')
     dummy = reshape(gaussian_data%xzts, (/igaus,jgaus/))
   case ('zc')
     dummy = reshape(gaussian_data%zc, (/igaus,jgaus/))
   case default
     print*,'- FATAL ERROR: UNKNOWN VAR IN GET_VAR: ', var
     call errexit(67)
 end select

 end subroutine get_netcdf_var

!-------------------------------------------------------------------------------------------
! Read tile data.
!-------------------------------------------------------------------------------------------

 subroutine read_data_anl

 use netcdf
 use io

 implicit none

 integer             :: ijtile, id_dim, id_var
 integer             :: error, tile, ncid
 integer             :: istart, iend

 real(kind=8), allocatable        :: dummy(:,:), dummy3d(:,:,:)

!-------------------------------------------------------------------------------------------
! Get tile dimensions from the first analysis file.
!-------------------------------------------------------------------------------------------

 error=nf90_open("./anal.tile1.nc",nf90_nowrite,ncid)
 error=nf90_inq_dimid(ncid, 'xaxis_1', id_dim)
 call netcdf_err(error, 'READING xaxis_1' )
 error=nf90_inquire_dimension(ncid,id_dim,len=itile)
 call netcdf_err(error, 'READING xaxis_1' )

 error=nf90_inq_dimid(ncid, 'yaxis_1', id_dim)
 call netcdf_err(error, 'READING yaxis_1' )
 error=nf90_inquire_dimension(ncid,id_dim,len=jtile)
 call netcdf_err(error, 'READING yaxis_1' )

 error = nf90_close(ncid)

 ijtile = itile*jtile

 allocate(dummy(itile,jtile))
 allocate(dummy3d(itile,jtile,4))

 allocate(tile_data%orog(ijtile*num_tiles))
 allocate(tile_data%canopy(ijtile*num_tiles))
 allocate(tile_data%slmask(ijtile*num_tiles))
 allocate(tile_data%tg3(ijtile*num_tiles))
 allocate(tile_data%alvsf(ijtile*num_tiles))
 allocate(tile_data%alvwf(ijtile*num_tiles))
 allocate(tile_data%alnsf(ijtile*num_tiles))
 allocate(tile_data%alnwf(ijtile*num_tiles))
 allocate(tile_data%facsf(ijtile*num_tiles))
 allocate(tile_data%facwf(ijtile*num_tiles))
 allocate(tile_data%ffhh(ijtile*num_tiles))
 allocate(tile_data%ffmm(ijtile*num_tiles))
 allocate(tile_data%fice(ijtile*num_tiles))
 allocate(tile_data%hice(ijtile*num_tiles))
 allocate(tile_data%sheleg(ijtile*num_tiles))
 allocate(tile_data%stype(ijtile*num_tiles))
 allocate(tile_data%vfrac(ijtile*num_tiles))
 allocate(tile_data%vtype(ijtile*num_tiles))
 allocate(tile_data%zorl(ijtile*num_tiles))
 allocate(tile_data%tsea(ijtile*num_tiles))
 allocate(tile_data%f10m(ijtile*num_tiles))
 allocate(tile_data%q2m(ijtile*num_tiles))
 allocate(tile_data%shdmax(ijtile*num_tiles))
 allocate(tile_data%shdmin(ijtile*num_tiles))
 allocate(tile_data%slope(ijtile*num_tiles))
 allocate(tile_data%snoalb(ijtile*num_tiles))
 allocate(tile_data%srflag(ijtile*num_tiles))
 allocate(tile_data%snwdph(ijtile*num_tiles))
 allocate(tile_data%t2m(ijtile*num_tiles))
 allocate(tile_data%tisfc(ijtile*num_tiles))
 allocate(tile_data%tprcp(ijtile*num_tiles))
 allocate(tile_data%uustar(ijtile*num_tiles))
 allocate(tile_data%slc(ijtile*num_tiles,4))
 allocate(tile_data%smc(ijtile*num_tiles,4))
 allocate(tile_data%stc(ijtile*num_tiles,4))
! nst
 if (donst) then
   allocate(tile_data%c0(ijtile*num_tiles))
   allocate(tile_data%cd(ijtile*num_tiles))
   allocate(tile_data%dconv(ijtile*num_tiles))
   allocate(tile_data%dtcool(ijtile*num_tiles))
   allocate(tile_data%land(ijtile*num_tiles))
   allocate(tile_data%qrain(ijtile*num_tiles))
   allocate(tile_data%tref(ijtile*num_tiles))
   allocate(tile_data%w0(ijtile*num_tiles))
   allocate(tile_data%wd(ijtile*num_tiles))
   allocate(tile_data%xs(ijtile*num_tiles))
   allocate(tile_data%xt(ijtile*num_tiles))
   allocate(tile_data%xtts(ijtile*num_tiles))
   allocate(tile_data%xu(ijtile*num_tiles))
   allocate(tile_data%xv(ijtile*num_tiles))
   allocate(tile_data%xz(ijtile*num_tiles))
   allocate(tile_data%xzts(ijtile*num_tiles))
   allocate(tile_data%zc(ijtile*num_tiles))
 endif

 do tile = 1, 6

   print*
   print*, "- READ INPUT SFC DATA FOR TILE: ", tile

   istart = (ijtile) * (tile-1) + 1
   iend   = istart + ijtile - 1

   if (tile==1) error=nf90_open("./anal.tile1.nc",nf90_nowrite,ncid)
   if (tile==2) error=nf90_open("./anal.tile2.nc",nf90_nowrite,ncid)
   if (tile==3) error=nf90_open("./anal.tile3.nc",nf90_nowrite,ncid)
   if (tile==4) error=nf90_open("./anal.tile4.nc",nf90_nowrite,ncid)
   if (tile==5) error=nf90_open("./anal.tile5.nc",nf90_nowrite,ncid)
   if (tile==6) error=nf90_open("./anal.tile6.nc",nf90_nowrite,ncid)

   call netcdf_err(error, 'OPENING FILE' )

   error=nf90_inq_varid(ncid, "slmsk", id_var)
   call netcdf_err(error, 'READING slmsk ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING slmsk' )
   print*,'- SLMSK: ',maxval(dummy),minval(dummy)
   tile_data%slmask(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "tsea", id_var)
   call netcdf_err(error, 'READING tsea ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING tsea' )
   print*,'- TSEA:  ',maxval(dummy),minval(dummy)
   tile_data%tsea(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "sheleg", id_var)
   call netcdf_err(error, 'READING sheleg ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING sheleg' )
   print*,'- SHELEG: ',maxval(dummy),minval(dummy)
   tile_data%sheleg(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "tg3", id_var)
   call netcdf_err(error, 'READING tg3 ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING tg3' )
   print*,'- TG3: ',maxval(dummy),minval(dummy)
   tile_data%tg3(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "zorl", id_var)
   call netcdf_err(error, 'READING zorl ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING zorl' )
   print*,'- ZORL: ',maxval(dummy),minval(dummy)
   tile_data%zorl(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "alvsf", id_var)
   call netcdf_err(error, 'READING alvsf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING alvsf' )
   print*,'- ALVSF: ',maxval(dummy),minval(dummy)
   tile_data%alvsf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "alvwf", id_var)
   call netcdf_err(error, 'READING alvwf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING alvwf' )
   print*,'- ALVWF: ',maxval(dummy),minval(dummy)
   tile_data%alvwf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "alnsf", id_var)
   call netcdf_err(error, 'READING alnsf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING alnsf' )
   print*,'- ALNSF: ',maxval(dummy),minval(dummy)
   tile_data%alnsf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "alnwf", id_var)
   call netcdf_err(error, 'READING alnwf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING alnwf' )
   print*,'- ALNWF: ',maxval(dummy),minval(dummy)
   tile_data%alnwf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "facsf", id_var)
   call netcdf_err(error, 'READING facsf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING facsf' )
   print*,'- FACSF: ',maxval(dummy),minval(dummy)
   tile_data%facsf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "facwf", id_var)
   call netcdf_err(error, 'READING facwf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING facwf' )
   print*,'- FACWF: ',maxval(dummy),minval(dummy)
   tile_data%facwf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "vfrac", id_var)
   call netcdf_err(error, 'READING vfrac ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING vfrac' )
   print*,'- VFRAC: ',maxval(dummy),minval(dummy)
   tile_data%vfrac(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "canopy", id_var)
   call netcdf_err(error, 'READING canopy ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING canopy' )
   print*,'- CANOPY: ',maxval(dummy),minval(dummy)
   tile_data%canopy(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "f10m", id_var)
   call netcdf_err(error, 'READING f10m ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING f10m' )
   print*,'- F10M: ',maxval(dummy),minval(dummy)
   tile_data%f10m(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "t2m", id_var)
   call netcdf_err(error, 'READING t2m ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING t2m' )
   print*,'- T2M: ',maxval(dummy),minval(dummy)
   tile_data%t2m(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "q2m", id_var)
   call netcdf_err(error, 'READING q2m ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING q2m' )
   print*,'- Q2M: ',maxval(dummy),minval(dummy)
   tile_data%q2m(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "vtype", id_var)
   call netcdf_err(error, 'READING vtype ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING vtype' )
   print*,'- VTYPE: ',maxval(dummy),minval(dummy)
   tile_data%vtype(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "stype", id_var)
   call netcdf_err(error, 'READING stype ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING stype' )
   print*,'- STYPE: ',maxval(dummy),minval(dummy)
   tile_data%stype(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "uustar", id_var)
   call netcdf_err(error, 'READING uustar ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING uustar' )
   print*,'- UUSTAR: ',maxval(dummy),minval(dummy)
   tile_data%uustar(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "ffmm", id_var)
   call netcdf_err(error, 'READING ffmm ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING ffmm' )
   print*,'- FFMM: ',maxval(dummy),minval(dummy)
   tile_data%ffmm(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "ffhh", id_var)
   call netcdf_err(error, 'READING ffhh ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING ffhh' )
   print*,'- FFHH: ',maxval(dummy),minval(dummy)
   tile_data%ffhh(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "hice", id_var)
   call netcdf_err(error, 'READING hice ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING hice' )
   print*,'- HICE: ',maxval(dummy),minval(dummy)
   tile_data%hice(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "fice", id_var)
   call netcdf_err(error, 'READING fice ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING fice' )
   print*,'- FICE: ',maxval(dummy),minval(dummy)
   tile_data%fice(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "tisfc", id_var)
   call netcdf_err(error, 'READING tisfc ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING tisfc' )
   print*,'- TISFC: ',maxval(dummy),minval(dummy)
   tile_data%tisfc(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "tprcp", id_var)
   call netcdf_err(error, 'READING tprcp ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING tprcp' )
   print*,'- TPRCP: ',maxval(dummy),minval(dummy)
   tile_data%tprcp(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "srflag", id_var)
   call netcdf_err(error, 'READING srflag ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING srfalg' )
   print*,'- SRFLAG: ',maxval(dummy),minval(dummy)
   tile_data%srflag(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "snwdph", id_var)
   call netcdf_err(error, 'READING snwdph ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING snwdph' )
   print*,'- SNWDPH: ',maxval(dummy),minval(dummy)
   tile_data%snwdph(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "shdmin", id_var)
   call netcdf_err(error, 'READING shdmin ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING shdmin' )
   print*,'- SHDMIN: ',maxval(dummy),minval(dummy)
   tile_data%shdmin(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "shdmax", id_var)
   call netcdf_err(error, 'READING shdmax ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING shdmax' )
   print*,'- SHDMAX: ',maxval(dummy),minval(dummy)
   tile_data%shdmax(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "slope", id_var)
   call netcdf_err(error, 'READING slope ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING slope' )
   print*,'- SLOPE: ',maxval(dummy),minval(dummy)
   tile_data%slope(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "snoalb", id_var)
   call netcdf_err(error, 'READING snoalb ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING snoalb' )
   print*,'- SNOALB: ',maxval(dummy),minval(dummy)
   tile_data%snoalb(istart:iend) = reshape(dummy, (/ijtile/))

   if (donst) then

     error=nf90_inq_varid(ncid, "c_0", id_var)
     call netcdf_err(error, 'READING c_0 ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING c_0' )
     print*,'- C_0: ',maxval(dummy),minval(dummy)
     tile_data%c0(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "c_d", id_var)
     call netcdf_err(error, 'READING c_d ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING c_d' )
     print*,'- C_D: ',maxval(dummy),minval(dummy)
     tile_data%cd(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "d_conv", id_var)
     call netcdf_err(error, 'READING d_conv ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING d_conv' )
     print*,'- D_CONV: ',maxval(dummy),minval(dummy)
     tile_data%dconv(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "dt_cool", id_var)
     call netcdf_err(error, 'READING dt_cool ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING dt_cool' )
     print*,'- DT_COOL: ',maxval(dummy),minval(dummy)
     tile_data%dtcool(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "qrain", id_var)
     call netcdf_err(error, 'READING qrain ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING qrain' )
     print*,'- QRAIN: ',maxval(dummy),minval(dummy)
     tile_data%qrain(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "tref", id_var)
     call netcdf_err(error, 'READING tref ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING tref' )
     print*,'- TREF: ',maxval(dummy),minval(dummy)
     tile_data%tref(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "w_0", id_var)
     call netcdf_err(error, 'READING w_0 ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING w_0' )
     print*,'- W_0: ',maxval(dummy),minval(dummy)
     tile_data%w0(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "w_d", id_var)
     call netcdf_err(error, 'READING w_d ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING w_d' )
     print*,'- W_D: ',maxval(dummy),minval(dummy)
     tile_data%wd(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xs", id_var)
     call netcdf_err(error, 'READING xs ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xs' )
     print*,'- XS: ',maxval(dummy),minval(dummy)
     tile_data%xs(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xt", id_var)
     call netcdf_err(error, 'READING xt ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xt' )
     print*,'- XT: ',maxval(dummy),minval(dummy)
     tile_data%xt(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xtts", id_var)
     call netcdf_err(error, 'READING xtts ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xtts' )
     print*,'- XTTS: ',maxval(dummy),minval(dummy)
     tile_data%xtts(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xzts", id_var)
     call netcdf_err(error, 'READING xzts ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xzts' )
     print*,'- XZTS: ',maxval(dummy),minval(dummy)
     tile_data%xzts(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xu", id_var)
     call netcdf_err(error, 'READING xu ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xu' )
     print*,'- XU: ',maxval(dummy),minval(dummy)
     tile_data%xu(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xv", id_var)
     call netcdf_err(error, 'READING xv ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xv' )
     print*,'- XV: ',maxval(dummy),minval(dummy)
     tile_data%xv(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xz", id_var)
     call netcdf_err(error, 'READING xz ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xz' )
     print*,'- XZ: ',maxval(dummy),minval(dummy)
     tile_data%xz(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "z_c", id_var)
     call netcdf_err(error, 'READING z_c ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING z_c' )
     print*,'- Z_C: ',maxval(dummy),minval(dummy)
     tile_data%zc(istart:iend) = reshape(dummy, (/ijtile/))

   endif  ! nst fields

   error=nf90_inq_varid(ncid, "smc", id_var)
   call netcdf_err(error, 'READING smc ID' )
   error=nf90_get_var(ncid, id_var, dummy3d)
   call netcdf_err(error, 'READING smc' )
   print*,'- SMC: ',maxval(dummy3d),minval(dummy3d)
   tile_data%smc(istart:iend,1:4) = reshape(dummy3d, (/ijtile,4/))

   error=nf90_inq_varid(ncid, "stc", id_var)
   call netcdf_err(error, 'READING stc ID' )
   error=nf90_get_var(ncid, id_var, dummy3d)
   call netcdf_err(error, 'READING stc' )
   print*,'- STC: ',maxval(dummy3d),minval(dummy3d)
   tile_data%stc(istart:iend,1:4) = reshape(dummy3d, (/ijtile,4/))

   error=nf90_inq_varid(ncid, "slc", id_var)
   call netcdf_err(error, 'READING slc ID' )
   error=nf90_get_var(ncid, id_var, dummy3d)
   call netcdf_err(error, 'READING slc' )
   print*,'- SLC: ',maxval(dummy3d),minval(dummy3d)
   tile_data%slc(istart:iend,1:4) = reshape(dummy3d, (/ijtile,4/))

   error = nf90_close(ncid)

   print*
   print*, "- READ INPUT OROG DATA FOR TILE: ",tile

   if (tile==1) error=nf90_open("./orog.tile1.nc",nf90_nowrite,ncid)
   if (tile==2) error=nf90_open("./orog.tile2.nc",nf90_nowrite,ncid)
   if (tile==3) error=nf90_open("./orog.tile3.nc",nf90_nowrite,ncid)
   if (tile==4) error=nf90_open("./orog.tile4.nc",nf90_nowrite,ncid)
   if (tile==5) error=nf90_open("./orog.tile5.nc",nf90_nowrite,ncid)
   if (tile==6) error=nf90_open("./orog.tile6.nc",nf90_nowrite,ncid)

   call netcdf_err(error, 'OPENING FILE' )

   if (fhr > 0.) then
      ! use filtered orog
      error=nf90_inq_varid(ncid, "orog_filt", id_var)
      call netcdf_err(error, 'READING orog_filt ID' )
      error=nf90_get_var(ncid, id_var, dummy)
      call netcdf_err(error, 'READING orog_filt' )
   else
      error=nf90_inq_varid(ncid, "orog_raw", id_var)
      call netcdf_err(error, 'READING orog_raw ID' )
      error=nf90_get_var(ncid, id_var, dummy)
      call netcdf_err(error, 'READING orog_raw' )
   endif
   print*,'- OROG: ',maxval(dummy),minval(dummy)
   tile_data%orog(istart:iend) = reshape(dummy, (/ijtile/))

   error = nf90_close(ncid)

 enddo

 deallocate (dummy, dummy3d)

 end subroutine read_data_anl

!-------------------------------------------------------------------------------------------
! Netcdf error routine.
!-------------------------------------------------------------------------------------------

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

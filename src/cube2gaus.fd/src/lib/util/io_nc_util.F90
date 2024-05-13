!-------------------------------------------------------------------------------
!> @brief wrapper of netcdf for io in ac project
!> @author Xi.Chen <xi.chen@noaa.gov>
!> @date 01/22/2016
!
!  REVISION HISTORY:
!  01/22/2016 - Initial Version
!-------------------------------------------------------------------------------

module io_nc_util_mod

  use netcdf

  implicit none
  private

  public :: io_nc_create_file
  public :: io_nc_close_file
  public :: io_nc_add_dim
  public :: io_nc_add_var_base
  public :: io_nc_put_var_real
  public :: io_nc_put_var_int
  public :: check

contains
  !=============================================================================
  !> @brief wrapper for nf90_create, return ncid
  integer function io_nc_create_file(ncfile_name) result(ncid)
    character (len=*), intent(in) :: ncfile_name
    call check( nf90_create(ncfile_name, nf90_clobber, ncid) )

    ! End define mode
    call check( nf90_enddef(ncid) )

  end function io_nc_create_file
  !=============================================================================
  !> @brief wrapper for nf90_close
  subroutine io_nc_close_file(ncid)
    integer, intent(in) :: ncid

    call check( nf90_close(ncid) )

  end subroutine io_nc_close_file
  !=============================================================================
  !> @brief description
  integer function io_nc_add_dim(ncid, dim_size, &
      dim_name, dim_longname, dim_unit, dim_cart_ax, dim_val) &
      result(dimid)
    integer, intent(in) :: ncid, dim_size
    character (len=*), intent(in) :: dim_name, dim_longname, &
        dim_unit, dim_cart_ax
    real, dimension(dim_size), intent(in) :: dim_val
    ! local
    integer :: varid

    ! Reopen define mode
    call check( nf90_redef(ncid) )

    ! Define dim
    call check( nf90_def_dim(ncid, dim_name, dim_size, dimid) )
    call check( nf90_def_var(ncid, dim_name, NF90_DOUBLE, dimid, varid) )
    call check( nf90_put_att(ncid, varid, "long_name", dim_longname) )
    call check( nf90_put_att(ncid, varid, "units", dim_unit) )
    call check( nf90_put_att(ncid, varid, "cartesian_axis", dim_cart_ax) )

    ! End define mode
    call check( nf90_enddef(ncid) )

    ! Write data
    call check( nf90_put_var(ncid, varid, dim_val) )

  end function io_nc_add_dim
  !=============================================================================
  !> @brief description
  integer function io_nc_add_var_base(ncid, dimids, &
      var_name, var_type, var_longname, var_unit, cell_methods, &
      fill_val) result(varid)
    integer, intent(in) :: ncid, var_type
    integer, dimension(:), intent(in) :: dimids
    real, intent(in) :: fill_val
    character (len=*), intent(in) :: var_name, var_longname, var_unit, &
      cell_methods

    ! Reopen define mode
    call check( nf90_redef(ncid) )

    call check( nf90_def_var(ncid, var_name, var_type, dimids, varid) )
    call check( nf90_put_att(ncid, varid, "long_name", var_longname) )
    call check( nf90_put_att(ncid, varid, "units", var_unit) )
    call check( nf90_put_att(ncid, varid, "missing_value", fill_val) )
    call check( nf90_put_att(ncid, varid, "_FILLValue", fill_val) )
    call check( nf90_put_att(ncid, varid, "cell_methods", cell_methods) )

    ! End define mode
    call check( nf90_enddef(ncid) )

  end function io_nc_add_var_base
  !=============================================================================
  !> @brief wrapper for nf90_put_var for real values
  subroutine io_nc_put_var_real(ncid,varid,var)
    integer, intent(in) :: ncid, varid
    real, dimension(:), intent(in) :: var

    call check( nf90_put_var(ncid, varid, var) )

  end subroutine io_nc_put_var_real
  !=============================================================================
  !> @brief wrapper for nf90_put_var for real values
  subroutine io_nc_put_var_int(ncid,varid,var)
    integer, intent(in) :: ncid, varid
    integer, dimension(:), intent(in) :: var

    call check( nf90_put_var(ncid, varid, var) )

  end subroutine io_nc_put_var_int
  !=============================================================================
  !> @brief description
  subroutine check(status)
    integer, intent(in) :: status

    if (status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    endif

  end subroutine check
  !=============================================================================
end module io_nc_util_mod


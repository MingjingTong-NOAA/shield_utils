!-------------------------------------------------------------------------------
!> @brief Main driver for FV3_DA utilities
!> @author Xi.Chen <xi.chen@noaa.gov>
!> @date 02/06/2016
!
!  REVISION HISTORY:
!  02/06/2016 - Initial Version
!  12/12/2019 - Combine fv_core, fv_tracer and phy_data into one file
!-------------------------------------------------------------------------------

module fv3_da_mod

  use fv3_da_ctrl_mod, only: get_parameters, get_weights_only, write_nemsio
  use interp_res_ncio_mod, only: interp_res_ncio

  use interp_res_nemsio_mod, only: interp_res_nemsio

  use fv_timing_mod, only: timing_on, timing_off, timing_prt

  use fv3_da_out_api_mod, only: fv3_da_out_type, &
                                fv3_da_out_init, &
                                fv3_da_out_end

  implicit none
  private

  public :: fv3_da

contains
  !=============================================================================
  !> @brief the main driver
  !> @author Xi.Chen <xi.chen@noaa.gov>
  !> @date 02/06/2016
  !> @Modified for SHiELD DA Mingjing.Tong <mingjing.tong@noaa.gov>
  subroutine fv3_da
  
    type(fv3_da_out_type) :: da
    !--------------------------------------------------------------------!
    ! main setup, read in namelists                                      !
    !--------------------------------------------------------------------!
    call timing_on('total')
    call get_parameters

    !--------------------------------------------------------------------!
    ! main setup, init from API                                          !
    !--------------------------------------------------------------------!
    call fv3_da_out_init(da)
   
    if (.not. get_weights_only) then
    !--------------------------------------------------------------------!
    ! do cub2latlon interpolation using API                              !
    !--------------------------------------------------------------------!
      call timing_on('interp_and_io')
      if (write_nemsio) then
         print *,'write nemsio'
         call interp_res_nemsio(da)
      else
         print *,'write netcdf'
         call interp_res_ncio(da)
      end if
      call timing_off('interp_and_io')
    end if
    !--------------------------------------------------------------------!
    ! deallocate arrays and clean up                                     !
    !--------------------------------------------------------------------!
    call fv3_da_out_end(da)
    call timing_off('total')
    call timing_prt(0)
  end subroutine fv3_da
  !=============================================================================
end module fv3_da_mod


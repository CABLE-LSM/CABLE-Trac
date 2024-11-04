  SUBROUTINE write_cnp_params(veg, casaflux, casamet)
    ! Input variables:
    !   landpt(mp)%type- via cable_IO_vars_module (%cstart,cend,ilon,ilat)
    !   patch(mp)%type - via cable_IO_vars_module (%frac)
    !   inSorder       - via cable_param_module
    !   inArea         - via cable_param_module
    !   inNdep         - via cable_param_module
    !   inNfix         - via cable_param_module
    !   inPdust        - via cable_param_module
    !   inPwea         - via cable_param_module

    USE casaparm, ONLY: cropland, croplnd2
    IMPLICIT NONE
    TYPE (veg_parameter_type),  INTENT(IN)    :: veg
    TYPE (casa_flux),           INTENT(INOUT) :: casaflux
    TYPE (casa_met),            INTENT(INOUT) :: casamet

    ! local variables
    INTEGER :: ee, hh

    DO ee=1, mland ! over all land grid points
       casamet%isorder(landpt(ee)%cstart:landpt(ee)%cend) =                     &
            inSorder(landpt(ee)%ilon,landpt(ee)%ilat)
       DO hh = landpt(ee)%cstart, landpt(ee)%cend  ! each patch in current grid
          casamet%lon(hh) = patch(hh)%longitude
          casamet%lat(hh) = patch(hh)%latitude
          casamet%areacell(hh) = patch(hh)%frac                                  &
               * inArea(landpt(ee)%ilon, landpt(ee)%ilat)
      ! YPW: the following lines are commented out.
      !    casaflux%Nmindep(hh) = patch(hh)%frac                                  &
      !        * inNdep(landpt(ee)%ilon, landpt(ee)%ilat)
      !    casaflux%Nminfix(hh) = patch(hh)%frac                                  &
      !         * inNfix(landpt(ee)%ilon, landpt(ee)%ilat)
      !    casaflux%Pdep(hh)    = patch(hh)%frac                                  &
      !         * inPdust(landpt(ee)%ilon, landpt(ee)%ilat)
      !    casaflux%Pwea(hh)    = patch(hh)%frac                                  &
      !         * inPwea(landpt(ee)%ilon, landpt(ee)%ilat)
          !! vh !! fluxes shouldn't be weighted by patch frac.
          !   IF (CABLE_USER%POPLUC) then
          casaflux%Nmindep(hh) =  inNdep(landpt(ee)%ilon, landpt(ee)%ilat)
          casaflux%Nminfix(hh) = MAX( inNfix(landpt(ee)%ilon, landpt(ee)%ilat), &
               8.0e-4)
          ! YPW this comment below is incorrect. As BNF rate is based on Peng et al. (2019), GBC, 10.1029/2019GB006296
          !vh ! minimum fixation rate of 3 kg N ha-1y-1 (8e-4 g N m-2 d-1)
          ! Cleveland, Cory C., et al. "Global patterns of terrestrial biological nitrogen (N2) &
          !fixation in natural ecosystems." Global biogeochemical cycles 13.2 (1999): 623-645.
          casaflux%Pdep(hh)    = inPdust(landpt(ee)%ilon, landpt(ee)%ilat)
          casaflux%Pwea(hh)    = inPwea(landpt(ee)%ilon, landpt(ee)%ilat)
          !  ENDIF

          ! fertilizer addition is included here
          IF (veg%iveg(hh) == cropland .OR. veg%iveg(hh) == croplnd2) THEN
             ! The following shoudl not multiplied by patch(hh)%frac
             ! P fertilizer =13 Mt P globally in 1994
             casaflux%Pdep(hh)    = casaflux%Pdep(hh)                             &
                  +  0.7 / 365.0
       !           + patch(hh)%frac * 0.7 / 365.0
             casaflux%Nmindep(hh) = casaflux%Nmindep(hh)                          &
                  +  4.0 / 365.0
       !           + patch(hh)%frac * 4.0 / 365.0
          ENDIF
       ENDDO
    ENDDO
    DEALLOCATE(inSorder, inArea, inNdep, inNfix, inPwea, inPdust)

    !write(668,*) 'in write_cnp_params: Ndep, Nfix', casaflux%Nmindep(1), casaflux%Nminfix(1)

  END SUBROUTINE write_cnp_params


! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module grib_fields_provider_mod
    
    use fckit_log_module, only : fckit_log
    use fckit_configuration_module, only : fckit_configuration
    use fckit_configuration_module, only : fckit_YAMLConfiguration
    
    use atlas_module, only : atlas_real
    use atlas_module, only : atlas_Field
    use atlas_module, only : atlas_FieldSet
    use atlas_module, only : atlas_functionspace_NodeColumns
    use atlas_module, only : atlas_functionspace_StructuredColumns
    use atlas_module, only : atlas_Grid
    use atlas_module, only : atlas_griddistribution
    use atlas_module, only : atlas_Vertical
    use atlas_module, only : atlas_Metadata

    use plume_module, only : plume_data
    use plume_module, only : plume_check

    use plugin_utils_mod, only : n_fields_srf
    use plugin_utils_mod, only : n_fields_cld
    use plugin_utils_mod, only : n_fields_spc
    use plugin_utils_mod, only : n_fields_oth
    use plugin_utils_mod, only : n_fields
#ifdef WITH_SCM_GRIB2_FIELDS
    use plugin_utils_mod, only : n_fields_sol
#endif

    use plugin_utils_mod, only : field_names_srf
    use plugin_utils_mod, only : field_names_cld
    use plugin_utils_mod, only : field_names_spc
    use plugin_utils_mod, only : field_names_oth
    use plugin_utils_mod, only : field_names
#ifdef WITH_SCM_GRIB2_FIELDS
    use plugin_utils_mod, only : field_names_sol
#endif

    use plugin_utils_mod, only : param_name2id
    
    use yomvar, only : jprd
    use yomvar, only : jprb
    use yomvar, only : jpim    
#ifdef WITH_SCM_GRIB2_FIELDS
    use yomvar, only : ncss
#endif

    implicit none
    
    private
    
    ! avoids allocatable array of allocatable char
    type field_name_t
        character(len=:), allocatable :: name
    contains

    end type
    
    
    type, public :: grib_fields_provider

        ! fields that we need to construct
        type(atlas_Field) :: fields_srf(n_fields_srf)
        type(atlas_Field) :: fields_cld(n_fields_cld)
        type(atlas_Field) :: fields_spc(n_fields_spc)
        ! type(atlas_Field) :: fields_oth(n_fields_oth)
#ifdef WITH_SCM_GRIB2_FIELDS
        ! Multi-level soil fields (sot/vsw/sit). Synthesized at setup time by
        ! repackaging the single-level stl/swvl/istl GRIB messages present in the
        ! surface GRIB fixture into ncss=4-level ATLAS fields.
        type(atlas_Field) :: fields_sol(n_fields_sol)
#endif

        ! fields u and v wind components
        type(atlas_Field) :: field_u
        type(atlas_Field) :: field_v
        type(atlas_Field) :: field_100u
        type(atlas_Field) :: field_100v
        type(atlas_Field) :: field_tcw
    
        logical :: initialised = .false.
    contains
    
        procedure, public  :: initialise => fields_provider__initialise
        procedure, public  :: setup => fields_provider__setup_fields
        procedure, public  :: provide_fields => fields_provider__provide_fields
        procedure, public  :: finalise => fields_provider__finalise

    end type
    
    contains
    
    
    subroutine fields_provider__initialise(this)
        class(grib_fields_provider), intent(inout) :: this
        if (this%initialised .eqv. .false.) then
            this%initialised = .true.
        else
            call fckit_log%info("*** grib_fields_provider already initialised! ***")
        endif
    end subroutine
    
    
    subroutine fields_provider__finalise(this)
        class(grib_fields_provider), intent(inout) :: this        
        if (this%initialised .eqv. .true.) then
            this%initialised = .false.
        else
            call fckit_log%info("*** grib_fields_provider already finalised! ***")
        endif    
    end subroutine 

    
    subroutine fields_provider__setup_fields(this, nlev, gridpoints, nodepoints, sfcfields, gpfields, gpfields_from_sp, windfield)

        class(grib_fields_provider),  intent(inout) :: this
        INTEGER(KIND=JPIM) :: nlev

        type(atlas_functionspace_StructuredColumns), intent(in) :: gridpoints
        type(atlas_functionspace_NodeColumns), intent(in) :: nodepoints
        type(atlas_FieldSet), intent(in) :: sfcfields
        type(atlas_FieldSet), intent(in) :: gpfields
        type(atlas_FieldSet), intent(in) :: gpfields_from_sp
        type(atlas_Field), intent(in) :: windfield

        type(atlas_Field) :: tmp_field
        type(atlas_Field) :: tmp_field_single_level
        type(atlas_Metadata) :: metadata

        REAL(KIND=JPRB), POINTER :: values(:,:)
        REAL(KIND=JPRB), POINTER :: values_single_level(:)

        REAL(KIND=JPRB), POINTER :: values_u(:,:)
        REAL(KIND=JPRB), POINTER :: values_v(:,:)
        REAL(KIND=JPRB), POINTER :: values_uv(:,:,:)

        integer :: ifield
        integer :: ifield_in_set
        integer :: ilvl
        integer :: iparam

        integer :: field_param_id

        ! Every field created below is zeroed before the GRIB records are copied in.
        ! Without this, a field whose paramId is absent from the input files is left
        ! at whatever create_field produced; in a Debug ATLAS build that is signalling
        ! NaN (ATLAS_INIT_SNAN), which traps under -fpe0 the first time the value goes
        ! through a real conversion - far away from here, in ncdf_varwrite1c.
        logical            :: found_any
        character(len=256) :: msg


        ! Create the SURFACE fields (and fill them in with value in the corresponding fieldset)
        ! write(*,*) "n_fields_srf: ", n_fields_srf
        do ifield=1,n_fields_srf

            field_param_id = param_name2id(trim(field_names_srf(ifield)))
            ! write(*,*) "CREATING FIELD: ", trim(field_names_srf(ifield)), " ==>> PARAM-ID: ", field_param_id

            tmp_field = gridpoints%create_field(name=trim(field_names_srf(ifield)), kind=atlas_real(jprb), type="scalar", levels=1)
            call tmp_field%data(values)
            values(:,:) = 0.0_jprb
            found_any = .false.

            ! loop over the fieldset (NOT very efficient, for testing only!)
            ! write(*,*) "sfcfields%size(): ", sfcfields%size()
            do ifield_in_set=1,sfcfields%size()
                
                tmp_field_single_level = sfcfields%field(ifield_in_set)                
                metadata = tmp_field_single_level%metadata()
                
                call metadata%get('paramId',iparam)
                call metadata%get('level',ilvl)

                ! write(*,*) " ----> iparam: ", iparam, " ----> lvl: ", ilvl

                if (iparam == field_param_id ) then
                    call tmp_field_single_level%data(values_single_level)
                    ! write(*,*) "size(values,1): ", size(values,1)
                    ! write(*,*) "size(values,2): ", size(values,2)
                    ! write(*,*) "size(values_single_level,1): ", size(values_single_level,1)
                    values(1,:) = values_single_level
                    found_any = .true.
                endif

            enddo

            if (.not.found_any) then
                write(msg,'(A,A,A,I0,A)') "grib_fields_provider: no GRIB record for surface field '", &
                  & trim(field_names_srf(ifield)), "' (paramId ", field_param_id, ") - zero filled"
                call fckit_log%warning(msg)
            endif

            this%fields_srf(ifield) = tmp_field
        enddo

#ifdef WITH_SCM_GRIB2_FIELDS
        ! Create the SOL fields (multi-level soil) by repackaging the
        ! constituent single-level GRIB messages that are still present in
        ! `sfcfields`. Each SOL field ends up with ncss=4 vertical layers, so
        ! the runtime plugin observes exactly the same values as in the
        ! single-level build mode -- just packed differently. Layout of
        ! sol_paramids: column j lists the four single-level paramIds that
        ! feed layer 1..4 of the j-th multi-level field (sot, vsw, sit).
        block
            integer, parameter :: sol_paramids(ncss, n_fields_sol) = reshape( [ &
              139, 170, 183, 236, &  ! sot <- stl1..stl4
               39,  40,  41,  42, &  ! vsw <- swvl1..swvl4
               35,  36,  37,  38  ], shape=[ncss, n_fields_sol] )  ! sit <- istl1..istl4
            integer :: isol_lvl

            do ifield=1,n_fields_sol

                tmp_field = gridpoints%create_field(name=trim(field_names_sol(ifield)), kind=atlas_real(jprb), type="scalar", levels=ncss)
                call tmp_field%data(values)
                values(:,:) = 0.0_jprb

                do isol_lvl=1,ncss
                    found_any = .false.
                    do ifield_in_set=1,sfcfields%size()
                        tmp_field_single_level = sfcfields%field(ifield_in_set)
                        metadata = tmp_field_single_level%metadata()
                        call metadata%get('paramId',iparam)
                        call metadata%get('level',ilvl)

                        if (iparam == sol_paramids(isol_lvl, ifield)) then
                            call tmp_field_single_level%data(values_single_level)
                            values(isol_lvl,:) = values_single_level
                            found_any = .true.
                        endif
                    enddo

                    if (.not.found_any) then
                        write(msg,'(A,A,A,I0,A,I0,A)') "grib_fields_provider: no GRIB record for soil field '", &
                          & trim(field_names_sol(ifield)), "' layer ", isol_lvl, " (paramId ", &
                          & sol_paramids(isol_lvl, ifield), ") - zero filled"
                        call fckit_log%warning(msg)
                    endif
                enddo

                this%fields_sol(ifield) = tmp_field
            enddo
        end block
#endif


        ! Create the CLOUD fields (and fill them in with value in the corresponding fieldset)
        ! write(*,*) "************** n_fields_cld: ", n_fields_cld
        do ifield=1,n_fields_cld
            
            field_param_id = param_name2id(trim(field_names_cld(ifield)))
            ! write(*,*) "CREATING FIELD: ", trim(field_names_cld(ifield)), " ==>> PARAM-ID: ", field_param_id

            tmp_field = gridpoints%create_field(name=trim(field_names_cld(ifield)), kind=atlas_real(jprb), type="scalar", levels=nlev)
            call tmp_field%data(values)
            values(:,:) = 0.0_jprb
            found_any = .false.

            ! loop over the fieldset (NOT very efficient, for testing only!)
            do ifield_in_set=1,gpfields%size()
                
                tmp_field_single_level = gpfields%field(ifield_in_set)                
                metadata = tmp_field_single_level%metadata()
                
                call metadata%get('paramId',iparam)
                call metadata%get('level',ilvl)

                ! write(*,*) " ----> iparam: ", iparam, " ----> lvl: ", ilvl

                if (iparam == field_param_id ) then
                    call tmp_field_single_level%data(values_single_level)
                    ! write(*,*) "size(values,1): ", size(values,1)
                    ! write(*,*) "size(values,2): ", size(values,2)
                    ! write(*,*) "size(values_single_level,1): ", size(values_single_level,1)
                    values(ilvl,:) = values_single_level
                    found_any = .true.
                endif
            enddo

            if (.not.found_any) then
                write(msg,'(A,A,A,I0,A)') "grib_fields_provider: no GRIB record for cloud field '", &
                  & trim(field_names_cld(ifield)), "' (paramId ", field_param_id, ") - zero filled"
                call fckit_log%warning(msg)
            endif

            this%fields_cld(ifield) = tmp_field
        enddo


        ! Create the GP fields from SP (and fill them in with value in the corresponding fieldset)
        do ifield=1,n_fields_spc
            
            field_param_id = param_name2id(trim(field_names_spc(ifield)))
            ! write(*,*) "CREATING FIELD: ", trim(field_names_spc(ifield)), " ==>> PARAM-ID: ", field_param_id

            ! geopotential and lnsp need a single level field
            if (field_param_id == 152 .or. field_param_id == 129) then
                tmp_field = nodepoints%create_field(name=trim(field_names_spc(ifield)), kind=atlas_real(jprb), type="scalar", levels=1)
            else
                tmp_field = nodepoints%create_field(name=trim(field_names_spc(ifield)), kind=atlas_real(jprb), type="scalar", levels=nlev)
            endif

            call tmp_field%data(values)
            values(:,:) = 0.0_jprb
            found_any = .false.

            ! loop over the fieldset (NOT very efficient, for testing only!)
            ! write(*,*) "gpfields_from_sp%size(): ", gpfields_from_sp%size()
            do ifield_in_set=1,gpfields_from_sp%size()
                
                tmp_field_single_level = gpfields_from_sp%field(ifield_in_set)                
                metadata = tmp_field_single_level%metadata()
                
                call metadata%get('paramId',iparam)
                call metadata%get('level',ilvl)

                if (iparam == field_param_id ) then
                    call tmp_field_single_level%data(values_single_level)
                    ! write(*,*) "size(values,1): ", size(values,1)
                    ! write(*,*) "size(values,2): ", size(values,2)
                    ! write(*,*) "size(values_single_level,1): ", size(values_single_level,1)
                    if (iparam == 152 .or. iparam == 129) then
                        values(1,:) = values_single_level
                    else
                        values(ilvl,:) = values_single_level
                    endif
                    found_any = .true.
                endif
            enddo

            ! The t159 fixtures carry no paramId 135 ("w"), so this fires for omega.
            if (.not.found_any) then
                write(msg,'(A,A,A,I0,A)') "grib_fields_provider: no GRIB record for spectral field '", &
                  & trim(field_names_spc(ifield)), "' (paramId ", field_param_id, ") - zero filled"
                call fckit_log%warning(msg)
            endif

            this%fields_spc(ifield) = tmp_field
        enddo


        ! U,V fields
        this%field_u = nodepoints%create_field(name="u", kind=atlas_real(JPRB), levels=nlev)
        call this%field_u%data(values_u)

        this%field_v = nodepoints%create_field(name="v", kind=atlas_real(JPRB), levels=nlev)
        call this%field_v%data(values_v)

        call windfield%data(values_uv)

        values_u(:,:) = values_uv(1,:,:)
        values_v(:,:) = values_uv(2,:,:)

        ! other fields (unused). No GRIB record is ever copied into these, so they must
        ! be zeroed explicitly - they would otherwise be handed to the plugin as sNaN.
#ifndef WITH_SCM_GRIB2_FIELDS
        ! 100u/100v are only expected by the plugin in the single-level (non-GRIB2) mode.
        this%field_100u = nodepoints%create_field(name="100u", kind=atlas_real(JPRB), levels=nlev)
        call this%field_100u%data(values)
        values(:,:) = 0.0_jprb

        this%field_100v = nodepoints%create_field(name="100v", kind=atlas_real(JPRB), levels=nlev)
        call this%field_100v%data(values)
        values(:,:) = 0.0_jprb
#endif
        this%field_tcw = nodepoints%create_field(name="tcw", kind=atlas_real(JPRB), levels=nlev)
        call this%field_tcw%data(values)
        values(:,:) = 0.0_jprb

    end subroutine


    subroutine fields_provider__provide_fields(this, data)
        class(grib_fields_provider),  intent(inout) :: this
        type(plume_data), intent(inout) :: data

        integer :: ifield

        do ifield=1,size(this%fields_srf)
          call plume_check( data%provide_atlas_field_shared(this%fields_srf(ifield)%name(), this%fields_srf(ifield)) )
        enddo
#ifdef WITH_SCM_GRIB2_FIELDS
        do ifield=1,size(this%fields_sol)
          call plume_check( data%provide_atlas_field_shared(this%fields_sol(ifield)%name(), this%fields_sol(ifield)) )
        enddo
#endif
        
        do ifield=1,size(this%fields_cld)
          call plume_check( data%provide_atlas_field_shared(this%fields_cld(ifield)%name(), this%fields_cld(ifield)) )
        enddo
        
        do ifield=1,size(this%fields_spc)
          call plume_check( data%provide_atlas_field_shared(this%fields_spc(ifield)%name(), this%fields_spc(ifield)) )
        enddo

        call plume_check( data%provide_atlas_field_shared("u", this%field_u) )
        call plume_check( data%provide_atlas_field_shared("v", this%field_v) )

#ifndef WITH_SCM_GRIB2_FIELDS
        call plume_check( data%provide_atlas_field_shared("100u", this%field_100u) )
        call plume_check( data%provide_atlas_field_shared("100v", this%field_100v) )
#endif
        call plume_check( data%provide_atlas_field_shared("tcw", this%field_tcw) )

        write(*,*) "finished providing data! "

    end subroutine
        


end module grib_fields_provider_mod
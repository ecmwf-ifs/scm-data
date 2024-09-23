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

    use plugin_utils_mod, only : field_names_srf
    use plugin_utils_mod, only : field_names_cld
    use plugin_utils_mod, only : field_names_spc
    use plugin_utils_mod, only : field_names_oth
    use plugin_utils_mod, only : field_names

    use plugin_utils_mod, only : param_name2id
    
    use yomvar, only : jprd
    use yomvar, only : jprb
    use yomvar, only : jpim    

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


        ! Create the SURFACE fields (and fill them in with value in the corresponding fieldset)
        ! write(*,*) "n_fields_srf: ", n_fields_srf
        do ifield=1,n_fields_srf

            field_param_id = param_name2id(trim(field_names_srf(ifield)))
            ! write(*,*) "CREATING FIELD: ", trim(field_names_srf(ifield)), " ==>> PARAM-ID: ", field_param_id

            tmp_field = gridpoints%create_field(name=trim(field_names_srf(ifield)), kind=atlas_real(jprb), type="scalar", levels=1)
            call tmp_field%data(values)

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
                endif

            enddo

            this%fields_srf(ifield) = tmp_field
        enddo



        ! Create the CLOUD fields (and fill them in with value in the corresponding fieldset)
        ! write(*,*) "************** n_fields_cld: ", n_fields_cld
        do ifield=1,n_fields_cld
            
            field_param_id = param_name2id(trim(field_names_cld(ifield)))
            ! write(*,*) "CREATING FIELD: ", trim(field_names_cld(ifield)), " ==>> PARAM-ID: ", field_param_id

            tmp_field = gridpoints%create_field(name=trim(field_names_cld(ifield)), kind=atlas_real(jprb), type="scalar", levels=nlev)
            call tmp_field%data(values)

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
                endif
            enddo

            this%fields_cld(ifield) = tmp_field
        enddo


        ! Create the GP fields from SP (and fill them in with value in the corresponding fieldset)
        ! write(*,*) "************** n_fields_spc: ", n_fields_spc
        do ifield=1,n_fields_spc
            
            field_param_id = param_name2id(trim(field_names_spc(ifield)))
            ! write(*,*) "CREATING FIELD: ", trim(field_names_spc(ifield)), " ==>> PARAM-ID: ", field_param_id

            tmp_field = nodepoints%create_field(name=trim(field_names_spc(ifield)), kind=atlas_real(jprb), type="scalar", levels=nlev)
            call tmp_field%data(values)

            ! loop over the fieldset (NOT very efficient, for testing only!)
            ! write(*,*) "gpfields_from_sp%size(): ", gpfields_from_sp%size()
            do ifield_in_set=1,gpfields_from_sp%size()
                
                tmp_field_single_level = gpfields_from_sp%field(ifield_in_set)                
                metadata = tmp_field_single_level%metadata()
                
                call metadata%get('paramId',iparam)
                call metadata%get('level',ilvl)

                ! write(*,*) " ----> iparam: ", iparam, " ----> lvl: ", ilvl

                if (iparam == field_param_id ) then
                    call tmp_field_single_level%data(values_single_level)
                    ! write(*,*) "size(values,1): ", size(values,1)
                    ! write(*,*) "size(values,2): ", size(values,2)
                    ! write(*,*) "size(values_single_level,1): ", size(values_single_level,1)

                    if (iparam == 129) then ! special case for Z values that are defined at level=0?
                        values(1,:) = values_single_level
                    else
                        values(ilvl,:) = values_single_level
                    endif
                endif
            enddo
            
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

        ! other fields (unused)
        this%field_100u = nodepoints%create_field(name="100u", kind=atlas_real(JPRB), levels=nlev)
        this%field_100v = nodepoints%create_field(name="100v", kind=atlas_real(JPRB), levels=nlev)
        this%field_tcw = nodepoints%create_field(name="tcw", kind=atlas_real(JPRB), levels=nlev)                        

    end subroutine


    subroutine fields_provider__provide_fields(this, data)
        class(grib_fields_provider),  intent(inout) :: this
        type(plume_data), intent(inout) :: data

        integer :: ifield

        do ifield=1,size(this%fields_srf)
          call plume_check( data%provide_atlas_field_shared(this%fields_srf(ifield)%name(), this%fields_srf(ifield)) )
        enddo
        
        do ifield=1,size(this%fields_cld)
          call plume_check( data%provide_atlas_field_shared(this%fields_cld(ifield)%name(), this%fields_cld(ifield)) )
        enddo
        
        do ifield=1,size(this%fields_spc)
          call plume_check( data%provide_atlas_field_shared(this%fields_spc(ifield)%name(), this%fields_spc(ifield)) )
        enddo

        call plume_check( data%provide_atlas_field_shared("u", this%field_u) )
        call plume_check( data%provide_atlas_field_shared("v", this%field_v) )

        call plume_check( data%provide_atlas_field_shared("100u", this%field_100u) )
        call plume_check( data%provide_atlas_field_shared("100v", this%field_100v) )
        call plume_check( data%provide_atlas_field_shared("tcw", this%field_tcw) )

        write(*,*) "finished providing data! "

    end subroutine
        


end module grib_fields_provider_mod
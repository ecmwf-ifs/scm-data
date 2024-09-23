! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

! -------------------------------------------------------------
! Read a field on grid points and return a field on node points
! -------------------------------------------------------------
function create_nodefield_from_field(field, nodepoints) result(field_nodes)
    use, intrinsic :: iso_C_binding
    use atlas_module, only: atlas_real
    use atlas_module, only: atlas_Field
    use atlas_module, only: atlas_functionspace_NodeColumns
    use atlas_module, only: atlas_mesh_Nodes
    use yomvar

    implicit none


    type(atlas_Field), intent(in) :: field
    type(atlas_functionspace_NodeColumns), intent(in) :: nodepoints
    type(atlas_mesh_Nodes) :: nodes
    type(atlas_Field) :: field_nodes
    type(atlas_Field) :: ghostField

#ifdef WITH_SCM_SINGLE_PRECISION
    REAL(KIND=c_float), POINTER :: values(:,:)
    REAL(KIND=c_float), POINTER :: nodedata(:,:)
#else
    REAL(KIND=c_double), POINTER :: values(:,:)
    REAL(KIND=c_double), POINTER :: nodedata(:,:)    
#endif

    INTEGER(KIND=c_int), POINTER :: ghost(:)

    integer :: ilev
    integer :: nlev
    integer :: inode
    integer :: jnode
    integer :: nb_nodes

    ! write(*,*) "Filling-in node points for field: ", field%name()

    ! N vertical levels
    nlev = field%levels()

    ! build field on nodes
    field_nodes = nodepoints%create_field(name=trim(field%name()), levels=nlev, kind=atlas_real(JPRB))
    call field_nodes%data(nodedata)
    nodes = nodepoints%nodes()
    ghostField = nodes%ghost()
    call ghostField%data(ghost)

    nb_nodes =  nodes%size()

    call field%data(values)

    inode=0
    do jnode=1,nb_nodes
        if ( ghost(jnode) /= 1 ) then
            inode=inode+1            
            do ilev = 1,nlev
                if (values(ilev,inode) .gt. 1e10) then
                    write(*,'(A,A,A,I0,A,I0,A,E)'), "from create_nodefield_from_field >>>>> field:", field%name(), ", inode:", inode, ", jnode:", jnode, ", val: ", values(ilev,inode)
                endif
                nodedata(ilev,jnode) = values(ilev,inode)
            enddo
        endif
    enddo
end function create_nodefield_from_field




! ------------------------------------------------------------------------
! Read an array of fields on grid points and return a field on node points
! where each field take the place of one variable in the created nodefield
! ------------------------------------------------------------------------
function create_nodefield_from_fields(name, fields, nodepoints) result(field_nodes)

    use, intrinsic :: iso_C_binding
    
    use atlas_module, only: atlas_real
    use atlas_module, only: atlas_Field
    use atlas_module, only: atlas_functionspace_NodeColumns
    use atlas_module, only: atlas_mesh_Nodes
    use yomvar
    
    implicit none
    
    
    type(atlas_Field), intent(in) :: fields(:)
    character(len=*), intent(in) :: name
    type(atlas_functionspace_NodeColumns), intent(in) :: nodepoints
    type(atlas_mesh_Nodes) :: nodes
    type(atlas_Field) :: field_nodes
    type(atlas_Field) :: ghostField
    type(atlas_Field) :: field_tmp
    
#ifdef WITH_SCM_SINGLE_PRECISION
    REAL(KIND=c_float), POINTER :: field_values(:,:)
    REAL(KIND=c_float), POINTER :: nodedata(:,:,:)
#else
    REAL(KIND=c_double), POINTER :: field_values(:,:)
    REAL(KIND=c_double), POINTER :: nodedata(:,:,:)    
#endif

    INTEGER(KIND=c_int), POINTER :: ghost(:)
    
    integer :: ilev
    integer :: nlev
    integer :: inode
    integer :: jnode
    integer :: nb_nodes
    integer :: nfields
    integer :: ifield
    
    ! write(*,*) "Filling-in node points for field: ", name
    
    ! N vertical levels 
    ! NOTE: here we assume that all fields have the same number of vertical levels!
    nlev = fields(1)%levels()

    ! n total fields
    nfields = size(fields)
    
    ! build field on nodes
    field_nodes = nodepoints%create_field(name=name, levels=nlev, kind=atlas_real(JPRB), variables=nfields)
    call field_nodes%data(nodedata)
    nodes = nodepoints%nodes()
    ghostField = nodes%ghost()
    call ghostField%data(ghost)
    
    nb_nodes =  nodes%size()
    
    inode=0
    do jnode=1,nb_nodes
        if ( ghost(jnode) /= 1 ) then
            inode=inode+1            
            do ilev = 1,nlev
                do ifield=1,nfields

                    field_tmp = fields(ifield)
                    call field_tmp%data(field_values)

                    if (field_values(ilev,inode) .gt. 1e10) then
                        write(*,'(A,A,A,I0,A,I0,A,E)'), "from create_nodefield_from_field >>>>> field:", field_tmp%name(), ", inode:", inode, ", jnode:", jnode, ", ifield: ", ifield,  ", val: ", field_values(ilev,inode)
                    endif
                    nodedata(ifield, ilev, jnode) = field_values(ilev, inode)
                enddo
            enddo
        endif
    enddo   
end function create_nodefield_from_fields
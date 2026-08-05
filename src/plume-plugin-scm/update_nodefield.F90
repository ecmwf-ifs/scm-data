! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


! Read a field on grid points and return a field on node points
! Note: this version updates an existing field
subroutine update_nodefield_from_field(field, nodepoints, field_nodes, ghost_mask)

    use, intrinsic :: iso_C_binding
    use atlas_module, only: atlas_real
    use atlas_module, only: atlas_Field
    use atlas_module, only: atlas_functionspace_NodeColumns
    use atlas_module, only: atlas_mesh_Nodes
    use yomvar

    implicit none


    type(atlas_Field), intent(in) :: field
    type(atlas_functionspace_NodeColumns), intent(in) :: nodepoints
    type(atlas_Field), intent(inout) :: field_nodes
    INTEGER(KIND=c_int), intent(in) :: ghost_mask(:)

#ifdef WITH_SCM_SINGLE_PRECISION
    REAL(KIND=c_float), POINTER :: values(:,:)
    REAL(KIND=c_float), POINTER :: nodedata(:,:)
#else
    REAL(KIND=c_double), POINTER :: values(:,:)
    REAL(KIND=c_double), POINTER :: nodedata(:,:)
#endif

    integer :: ilev
    integer :: nlev
    integer :: inode
    integer :: jnode
    integer :: nb_nodes

    ! N vertical levels
    nlev = field%levels()

    ! build field on nodes

    call field_nodes%data(nodedata)
    nb_nodes = size(ghost_mask)

    call field%data(values)

    inode=0
    do jnode=1,nb_nodes
        if ( ghost_mask(jnode) /= 1 ) then
            inode=inode+1
            do ilev = 1,nlev
                nodedata(ilev,jnode) = values(ilev,inode)
            enddo
        endif
    enddo

end subroutine update_nodefield_from_field




! ------------------------------------------------------------------------
! Read an array of fields on grid points and return a field on node points
! where each field take the place of one variable in the created nodefield
! ------------------------------------------------------------------------
subroutine update_nodefield_from_fields(fields, nodepoints, field_nodes, ghost_mask)

    use, intrinsic :: iso_C_binding

    use atlas_module, only: atlas_real
    use atlas_module, only: atlas_Field
    use atlas_module, only: atlas_functionspace_NodeColumns
    use atlas_module, only: atlas_mesh_Nodes
    use yomvar

    implicit none

    type(atlas_Field), intent(in) :: fields(2)
    type(atlas_functionspace_NodeColumns), intent(in) :: nodepoints
    type(atlas_Field), intent(inout) :: field_nodes
    INTEGER(KIND=c_int), intent(in) :: ghost_mask(:)

    type(atlas_Field) :: field_tmp

#ifdef WITH_SCM_SINGLE_PRECISION
    REAL(KIND=c_float), POINTER :: field_values(:,:)
    REAL(KIND=c_float), POINTER :: nodedata(:,:,:)
#else
    REAL(KIND=c_double), POINTER :: field_values(:,:)
    REAL(KIND=c_double), POINTER :: nodedata(:,:,:)
#endif

    integer :: ilev
    integer :: nlev
    integer :: inode
    integer :: jnode
    integer :: nb_nodes
    integer :: nfields
    integer :: ifield

    ! N vertical levels
    ! NOTE: here we assume that all fields have the same number of vertical levels!
    nlev = fields(1)%levels()

    ! n total fields
    nfields = size(fields)

    ! build field on nodes
    call field_nodes%data(nodedata)
    nb_nodes = size(ghost_mask)


    do ifield=1,nfields

        field_tmp = fields(ifield)
        call field_tmp%data(field_values)

        inode=0
        do jnode=1,nb_nodes
            if ( ghost_mask(jnode) /= 1 ) then
                inode=inode+1
                do ilev = 1,nlev
                    nodedata(ifield, ilev, jnode) = field_values(ilev, inode)
                enddo
            endif
        enddo
    enddo

    call field_tmp%final()

end subroutine update_nodefield_from_fields

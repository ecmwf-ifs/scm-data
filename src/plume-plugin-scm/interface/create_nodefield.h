! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

INTERFACE

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
end function create_nodefield_from_field




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
end function create_nodefield_from_fields

END INTERFACE
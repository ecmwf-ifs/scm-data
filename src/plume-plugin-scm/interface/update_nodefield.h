! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http:!www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

INTERFACE


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

end subroutine update_nodefield_from_field


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

end subroutine update_nodefield_from_fields

END INTERFACE
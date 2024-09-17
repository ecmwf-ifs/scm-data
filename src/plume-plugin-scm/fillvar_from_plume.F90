SUBROUTINE FILLVAR_FROM_PLUME(myproc,ilocation, gpfields)

use atlas_module
use yomvar
use plugin_utils_mod, only : param_name2id

IMPLICIT NONE
INTEGER(KIND=JPIM), intent(in) :: myproc
TYPE(TLOCATION), target, intent(inout) :: ilocation
type(atlas_FieldSet), intent(in) :: gpfields


INTEGER(KIND=JPIM) :: IPARAM, JFLD, ISIZE, knode, i
type(atlas_Field) :: field
type(atlas_Metadata) :: metadata
TYPE(TPARAM), POINTER :: PX

REAL(KIND=JPRB),POINTER :: values(:,:)

PX => ilocation%PP
knode = ilocation%iloc

isize = gpfields%size()

DO JFLD=1,ISIZE

  field = gpfields%field(JFLD)
  metadata = field%metadata()
  
  call field%data(values)
!   call metadata%get('paramId',iparam)
  iparam = param_name2id(field%name())

  SELECT CASE (iparam)
  CASE (139)
     PX%PSTL(1)=values(1,knode)
  CASE (170)
     PX%PSTL(2)=values(1,knode)
  CASE (183)
     PX%PSTL(3)=values(1,knode)
  CASE (236)
     PX%PSTL(4)=values(1,knode)
  CASE (39)
     PX%PSWL(1)=values(1,knode)
  CASE (40)
     PX%PSWL(2)=values(1,knode)
  CASE (41)
     PX%PSWL(3)=values(1,knode)
  CASE (42)
     PX%PSWL(4)=values(1,knode)
  CASE (141)
     PX%PSD   =values(1,knode)
  CASE (198)
     PX%PSRC  =values(1,knode)
  CASE (235)
     PX%PSKT  =values(1,knode)
  CASE (238)
     PX%PTSN  =values(1,knode)
  CASE (32)
     PX%PASN  =values(1,knode)
  CASE (33)
    PX%PRSN  =values(1,knode)
  CASE (172)
   !   write(*,*) ' land-sea mask ' , myproc, knode, values(1,knode)
     PX%PLSM  =values(1,knode)
  CASE (173)
     PX%PSR   =values(1,knode)
  CASE (234)
     PX%PLSRH =values(1,knode)
  CASE (174)
    PX%PAL   =values(1,knode)
  CASE (74)
     PX%PSDFOR=values(1,knode)
  CASE (160)
     PX%PSDOR =values(1,knode)
  CASE (161)
     PX%PISOR =values(1,knode)
  CASE (162)
     PX%PANOR =values(1,knode)
  CASE (163)
     PX%PSLOR =values(1,knode)
  CASE(31)  
     PX%PCI   =values(1,knode)
  CASE(34)
     PX%PSST  =values(1,knode)
  CASE(30)
     PX%PTVH  =values(1,knode)
  CASE(29)
     PX%PTVL  =values(1,knode)
  CASE(28)
     PX%PCVH  =values(1,knode)
  CASE(27)
     PX%PCVL  =values(1,knode)
  CASE(35)
     PX%PTIA(1)=values(1,knode)
  CASE(36)
     PX%PTIA(2)=values(1,knode)
  CASE(37)
     PX%PTIA(3)=values(1,knode)
  CASE(38)
     PX%PTIA(4)=values(1,knode)
  CASE(146)
     PX%PSSHF =values(1,knode)
  CASE(147)
     PX%PSLHF =values(1,knode)
  CASE(15)
     PX%PALUVP=values(1,knode)
  CASE(16)
     PX%PALUVD=values(1,knode)
  CASE(17)
     PX%PALNIP=values(1,knode)
  CASE(18)
     PX%PALNID=values(1,knode)
  CASE(66)
     PX%PLAILC=values(1,knode)
  CASE(67)
     PX%PLAIHC=values(1,knode)
  CASE(43)
     PX%PSOTY =values(1,knode)
  CASE(26)
     PX%PLAKEFR =values(1,knode)
  CASE(228007)
     PX%PLAKEDL=values(1,knode)
  CASE(228008)
     PX%PLAKEMLT=values(1,knode)
  CASE(228009)
     PX%PLAKEMLD=values(1,knode)
  CASE(228010)
     PX%PLAKEBLT=values(1,knode)
  CASE(228011)
     PX%PLAKETLT=values(1,knode)
  CASE(228012)
     PX%PLAKESHF=values(1,knode)
  CASE(228013)
     PX%PLAKEICT=values(1,knode)
  CASE(228014)
     PX%PLAKEICD=values(1,knode)
  CASE DEFAULT
     WRITE(*,*)  ' FILLVAR, WARNING: UNKNOWN FIELD PARAMETER ',IPARAM
  END SELECT

ENDDO

END SUBROUTINE FILLVAR_FROM_PLUME

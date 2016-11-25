SUBROUTINE FILLVAR(myproc,ilocation, gpfields)

use atlas_module
use yomvar

IMPLICIT NONE
INTEGER(KIND=JPIM), intent(in) :: myproc
TYPE(TLOCATION), target, intent(inout) :: ilocation
type(atlas_FieldSet), intent(in) :: gpfields


INTEGER(KIND=JPIM) :: IPARAM, JFLD, ISIZE, knode, i
type(atlas_Field) :: field
type(atlas_Metadata) :: metadata
TYPE(TPARAM), POINTER :: PX

REAL(KIND=JPRB),POINTER :: values(:)

PX => ilocation%PP
knode = ilocation%iloc

isize = gpfields%size()
DO JFLD=1,ISIZE

  field = gpfields%field(JFLD)
  metadata = field%metadata()
  call field%data(values)
  call metadata%get('paramId',iparam)
  SELECT CASE (iparam)
  CASE (139)
     PX%PSTL(1)=values(knode)
     !write(*,*) 'values size ', size(values)
     !do i=1,size(values)
     !  write(*,*) 'check ', i, values(i)
     !enddo
  CASE (170)
     PX%PSTL(2)=values(knode)
  CASE (183)
     PX%PSTL(3)=values(knode)
  CASE (236)
     PX%PSTL(4)=values(knode)
  CASE (39)
     PX%PSWL(1)=values(knode)
  CASE (40)
     PX%PSWL(2)=values(knode)
  CASE (41)
     PX%PSWL(3)=values(knode)
  CASE (42)
     PX%PSWL(4)=values(knode)
  CASE (141)
     PX%PSD   =values(knode)
  CASE (198)
     PX%PSRC  =values(knode)
  CASE (235)
     PX%PSKT  =values(knode)
  CASE (238)
     PX%PTSN  =values(knode)
  CASE (32)
     PX%PASN  =values(knode)
  CASE (33)
    PX%PRSN  =values(knode)
  CASE (172)
     write(*,*) ' land-sea mask ' , myproc, knode, values(knode)
     PX%PLSM  =values(knode)
  CASE (173)
     PX%PSR   =values(knode)
  CASE (234)
     PX%PLSRH =values(knode)
  CASE (174)
    PX%PAL   =values(knode)
  CASE (74)
     PX%PSDFOR=values(knode)
  CASE (160)
     PX%PSDOR =values(knode)
  CASE (161)
     PX%PISOR =values(knode)
  CASE (162)
     PX%PANOR =values(knode)
  CASE (163)
     PX%PSLOR =values(knode)
  CASE(31)  
     PX%PCI   =values(knode)
  CASE(34)
     PX%PSST  =values(knode)
  CASE(30)
     PX%PTVH  =values(knode)
  CASE(29)
     PX%PTVL  =values(knode)
  CASE(28)
     PX%PCVH  =values(knode)
  CASE(27)
     PX%PCVL  =values(knode)
  CASE(35)
     PX%PTIA(1)=values(knode)
  CASE(36)
     PX%PTIA(2)=values(knode)
  CASE(37)
     PX%PTIA(3)=values(knode)
  CASE(38)
     PX%PTIA(4)=values(knode)
  CASE(146)
     PX%PSSHF =values(knode)
  CASE(147)
     PX%PSLHF =values(knode)
  CASE(15)
     PX%PALUVP=values(knode)
  CASE(16)
     PX%PALUVD=values(knode)
  CASE(17)
     PX%PALNIP=values(knode)
  CASE(18)
     PX%PALNID=values(knode)
  CASE(66)
     PX%PLAILC=values(knode)
  CASE(67)
     PX%PLAIHC=values(knode)
  CASE(43)
     PX%PSOTY =values(knode)
  CASE(26)
     PX%PLAKEFR =values(knode)
  CASE(228007)
     PX%PLAKEDL=values(knode)
  CASE(228008)
     PX%PLAKEMLT=values(knode)
  CASE(228009)
     PX%PLAKEMLD=values(knode)
  CASE(228010)
     PX%PLAKEBLT=values(knode)
  CASE(228011)
     PX%PLAKETLT=values(knode)
  CASE(228012)
     PX%PLAKESHF=values(knode)
  CASE(228013)
     PX%PLAKEICT=values(knode)
  CASE(228014)
     PX%PLAKEICD=values(knode)
  CASE DEFAULT
     WRITE(*,*)  ' FILLVAR, WARNING: UNKNOWN FIELD PARAMETER ',IPARAM
  END SELECT

ENDDO

END SUBROUTINE FILLVAR

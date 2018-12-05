SUBROUTINE READ_GRIB_SCM(LSINGLE, MYPROC, FILE,LPROGNOSTIC,LAREA,INFO, &
 & spectral, spfields, gridpoints, gpfields)

! fieldset spfields for spectral
! fieldset gpfields for gridpoint

!    lprognostic    I   .TRUE. if only prognostic variables are needed
!    LAREA          I   .TRUE.  if lat-lon rectangle selection
!                       .FALSE. if one or more points, via its coordinates

use, intrinsic :: iso_C_binding

use fckit_mpi_module, only : fckit_mpi_comm
use fckit_log_module, only : log
use atlas_module, only : atlas_Field, atlas_FieldSet, atlas_functionspace_StructuredColumns, atlas_functionspace_Spectral, &
 & atlas_Metadata, atlas_real 
USE GRIB_API, only : GRIB_READ_FROM_FILE, GRIB_NEW_FROM_MESSAGE, GRIB_GET, GRIB_GET_SIZE, GRIB_RELEASE, & 
 & GRIB_OPEN_FILE, GRIB_COUNT_IN_FILE, GRIB_CLOSE_FILE, GRIB_SUCCESS
USE MPL_MODULE, only : mpl_init, mpl_broadcast, mpl_send, &
& mpl_recv, mpl_barrier, mpl_wait, jp_non_blocking_standard, jp_blocking_standard
use yomvar

IMPLICIT NONE

! testing dimensions
INTEGER(KIND=JPIM), PARAMETER :: JPMAXGRID = 1280_JPIM*640_JPIM
!INTEGER(KIND=JPIM), PARAMETER :: JPMAXGRID = 5120_JPIM*2560_JPIM

LOGICAL, intent(in) :: LSINGLE
INTEGER(KIND=JPIM),intent(in) :: MYPROC
CHARACTER(len=30), intent(in) :: FILE

type(atlas_FieldSet), intent(inout) :: spfields
type(atlas_FieldSet), intent(inout) :: gpfields
type(atlas_functionspace_StructuredColumns), intent(in) :: gridpoints
type(atlas_functionspace_Spectral), intent(in) :: spectral

TYPE(TINFO), intent(inout) :: INFO
LOGICAL, intent(in) :: lprognostic,larea

type(fckit_mpi_comm) :: mpi_comm

REAL(KIND=JPRB) :: ZINDEF
     
INTEGER(KIND=JPIM) :: IGRIB_IN, ibitmap
INTEGER(KIND=JPIM) :: ISTARTSTEP, IENDSTEP
CHARACTER*127 CGRIDTYPE,CLEVTYPE,CSTEPTYPE
CHARACTER*127 msg

INTEGER(KIND=JPIM) :: j, iret, ifi, ifo, ilenf, iparam, ilev, idate, itime, istep, itag, iflds, isp, itot, jfld

character(len=10) :: fieldname, str
type(atlas_Field) :: field, fieldg, fields, fields_input, fields_local
type(atlas_Metadata) :: metadata
INTEGER(KIND=JPIM) :: NPROC, JMAX

! define parallel IO
type(atlas_FieldSet) :: spset, gpset
INTEGER(KIND=JPIM), ALLOCATABLE :: IOPROC(:), ITEST(:), IASEND(:), IPAR(:,:), ILEVEL(:,:), ILPARAM(:), ILLEV(:)
INTEGER(KIND=JPIM), ALLOCATABLE ::  IOTAG(:), IOROOT(:), IFIELD(:,:), IMAXF(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: IGRIBUF(:), IRECBUF(:), ISENDREQ(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: ILENSEND(:), IOFFSEND(:), IBYTES(:), NGSTA(:)
INTEGER(KIND=JPIM) :: IOFF, ICOUNT, ISEND, ISIZEBUF, NFIELDS, ILEN_BYTES, ILRECV, IMAXFLDS, ISIZE, IMAXREC
INTEGER(KIND=JPIM) :: IOMASTER, IO_SHIFT, IO
LOGICAL :: LLFIRST 

LOGICAL, ALLOCATABLE :: LLTYPE(:,:), LLSPECTRAL(:)

REAL(KIND=c_double), POINTER :: fieldgdata(:)
REAL(KIND=c_double), POINTER :: fieldsdata(:,:), zdata(:), gribsdata(:)
REAL(KIND=c_double), POINTER :: locdata(:)

#include "dist_grid.h"

IOMASTER=1_JPIM
! set to 1 if not to use IOMASTER FOR DECODING
IO_SHIFT = 0
mpi_comm = fckit_mpi_comm()
NPROC = mpi_comm%size() 
IF( NPROC > 1 ) IO_SHIFT = 1
IF( NPROC > 1 ) CALL MPL_INIT()
ITAG = 123456
IRET = 0
str=''

if( .not.(spectral%is_null()) ) then
  JMAX = MIN(NPROC-IO_SHIFT,4_JPIM)
!  IO_SHIFT = 0
else
  JMAX = MIN(NPROC-IO_SHIFT,20_JPIM)
endif
NFIELDS=0
IF( MYPROC == IOMASTER ) THEN
  ! open file
  str = TRIM(FILE)
  write(*,*) 'opening ', str
  CALL GRIB_OPEN_FILE(IFI,str,'R')
  CALL GRIB_COUNT_IN_FILE(IFI,NFIELDS)
ENDIF

IF( NPROC > 1 ) THEN
  CALL MPL_BROADCAST(NFIELDS,KROOT=IOMASTER,KTAG=ITAG, CDSTRING='GRIB_INFO:')
ENDIF

IF( NFIELDS > 0 ) THEN
  ALLOCATE(IASEND(3))
  ALLOCATE(IOROOT(NFIELDS))
  ALLOCATE(IOTAG(NFIELDS))
 ALLOCATE(IFIELD(NFIELDS,JMAX+IO_SHIFT))
  ALLOCATE(LLTYPE(NFIELDS,JMAX+IO_SHIFT))
  ALLOCATE(IPAR(NFIELDS,JMAX+IO_SHIFT))
  IPAR(:,:) = 0
  ALLOCATE(ILPARAM(NFIELDS))
  ALLOCATE(ILEVEL(NFIELDS,JMAX+IO_SHIFT))
  ILEVEL(:,:) = 0
  ALLOCATE(ILLEV(NFIELDS))

  ALLOCATE(LLSPECTRAL(NFIELDS))
  LLTYPE(:,:) = .FALSE.
  ALLOCATE(IOPROC(JMAX))
  ALLOCATE(ITEST(JMAX))
  ALLOCATE(IMAXF(JMAX))
  IOPROC(:) = -1
ELSE
  write(*,*) ' error reading file: ', str
  CALL EXIT(1)
ENDIF

IF( LSINGLE ) THEN
  IOPROC(:) = 1
  IOTAG(:) = ITAG + 1
  IOROOT(:) =  1
ELSE
  DO J=1, JMAX
     IOPROC(J) = J + IO_SHIFT
  ENDDO
  ! needed for broadcast
  IMAXREC=0
  IMAXF(:) = 0
  DO IFLDS=1, NFIELDS
    DO J=1, JMAX
      IF ( IFLDS == (IFLDS/JMAX)*JMAX + MOD(J,JMAX) ) THEN
        IOROOT(IFLDS) =  IOPROC(J)
        IOTAG(IFLDS) = ITAG +  IFLDS
        IO = IOROOT(IFLDS) - IO_SHIFT
        IMAXF(IO) = IMAXF(IO) + 1
        IFIELD(IMAXF(IO),IOPROC(J)) = IFLDS
      ENDIF
    ENDDO
    IF ( IOROOT(IFLDS) ==  MYPROC ) THEN
      IO = IOROOT(IFLDS) - IO_SHIFT
      IMAXREC = IMAXF(IO)
    ENDIF
  ENDDO
ENDIF

IF( NPROC > 1 ) THEN
  DO j=1,JMAX
    ITAG= 643210 + J
    IO = IOPROC(J)
    CALL MPL_BROADCAST(IMAXF(J),KROOT=IO,KTAG=ITAG, CDSTRING='GRIB_INFO1:')
  ENDDO
ENDIF

ISIZE = JPMAXGRID + 11000_JPIM
!ISIZE = 200
ITEST(:) = MYPROC

! these IO procs should receive from proc 1 to fill buffers with grib messages

IF( ANY(ITEST .EQ. IOPROC) .OR. MYPROC == IOMASTER ) THEN

  write(*,*) 'number of fields :', NFIELDS/JMAX + 1 , NFIELDS, IMAXREC
  ISEND=1
  IMAXFLDS = MAX(NPROC,1)

  ! sending
  IF( MYPROC == IOMASTER ) THEN
    ALLOCATE(IGRIBUF(ISIZE*IMAXFLDS))
    ISIZEBUF=SIZE(IGRIBUF)
    ALLOCATE(ISENDREQ(NFIELDS+1))
    ALLOCATE(IBYTES(NFIELDS))
    ALLOCATE(ILENSEND(NFIELDS+1))
    ALLOCATE(IOFFSEND(NFIELDS+1))
    IOFFSEND(1)=0
  ENDIF

  ! receiving
  IF( ANY(ITEST .EQ. IOPROC) ) THEN
    ALLOCATE(IRECBUF(IMAXREC*ISIZE))
    ALLOCATE(NGSTA(IMAXREC+1))
    NGSTA(1) = 1_JPIM
    NGSTA(2:) = -HUGE(ISIZE)
    ICOUNT=0
    ILRECV=0
  ENDIF
  
  READ: DO IFLDS=1,NFIELDS

    IF( MYPROC == IOMASTER ) THEN

!      ILEN_BYTES = (ISIZEBUF-IOFFSEND(ISEND))*4
      ILEN_BYTES = ISIZEBUF*4
      CALL GRIB_READ_FROM_FILE(IFI,IGRIBUF(IOFFSEND(ISEND)+1:),ILEN_BYTES,IRET)
      if( iret == -3 ) then
        write(*,*) 'wrong buffer size iret= ', iret
      endif
      IBYTES(IFLDS)=ILEN_BYTES
      IF( IRET < 0 ) THEN
        write(*,*) 'end of file'
        ILENSEND(ISEND)=1
      else
        ! convert bytes into integer record length
        ILENSEND(ISEND)=(IBYTES(IFLDS)+4-1)/4
      endif
      IOFF=IOFFSEND(ISEND)
      IOFFSEND(ISEND+1)=IOFF+ILENSEND(ISEND)
      IF( IOROOT(IFLDS) /= MYPROC .AND. ILENSEND(ISEND) > 1 ) THEN
        CALL MPL_SEND(IGRIBUF(IOFF+1:IOFF+ILENSEND(ISEND)), KDEST=IOROOT(IFLDS), KTAG=IOTAG(IFLDS), & 
         & KMP_TYPE=JP_BLOCKING_STANDARD, KREQUEST=ISENDREQ(ISEND), CDSTRING='SEND MSG')
 !       write(*,*) 'sending: ', ISEND, ILENSEND(ISEND), IOROOT(IFLDS), IOTAG(IFLDS)
      ENDIF
      ISEND=ISEND+1
      
    ENDIF

    IF ( IOROOT(IFLDS) ==  MYPROC ) THEN
      ICOUNT=ICOUNT+1
      IF( MYPROC /= IOMASTER ) THEN
        CALL MPL_RECV(IRECBUF(NGSTA(ICOUNT):),KSOURCE=IOMASTER,KTAG=IOTAG(IFLDS), &
         & KOUNT=ILRECV,KMP_TYPE=JP_BLOCKING_STANDARD,CDSTRING='RECV FIELDS')
 !       write(*,*) 'recv: ', ICOUNT, ILRECV,  MYPROC, IOTAG(IFLDS)
      ELSE
        ILRECV=ILENSEND(ISEND-1)
        IRECBUF(NGSTA(ICOUNT):NGSTA(ICOUNT)+ILRECV-1) = IGRIBUF(IOFF+1:IOFF+ILENSEND(ISEND-1))
      ENDIF
      NGSTA(ICOUNT+1)=NGSTA(ICOUNT)+ILRECV
      ILRECV=0
    ENDIF

    IF( ISEND > IMAXFLDS  ) THEN
!      IF ( MYPROC == IOMASTER ) THEN
!     CALL MPL_WAIT(IGRIBUF(:),KREQUEST=ISENDREQ(1:ISEND), CDSTRING='WAIT')
!      ENDIF
      ISEND=1
    ENDIF
    
  ENDDO READ
  
ENDIF

!
!     2. DECODING LOOP
!
!  bit-mapped fields (SST and CI)

ZINDEF=19591204._JPRB

LLFIRST=.TRUE.
IFLDS=0
ICOUNT=0
IGRIB_IN=-1

spset = atlas_FieldSet("spectral")
gpset = atlas_FieldSet("gridpoints")

if( .not.(spectral%is_null()) ) then
  DO IFLDS=1,NFIELDS
    write(fieldname,'(I0)') IFLDS
    fields = spectral%create_field(fieldname,atlas_real(JPRB), global=.true.,owner= IOROOT(IFLDS)-1 )
    call spset%add( fields )
  ENDDO
endif
if( .not.(gridpoints%is_null()) ) then
  DO IFLDS=1,NFIELDS
    fieldg = gridpoints%create_field(fieldname,atlas_real(JPRB), global=.true.,owner= IOROOT(IFLDS)-1 )
    call gpset%add( fieldg )
  ENDDO
endif
  
DECODE: DO JFLD=1,IMAXREC
  
  IFLDS = IFIELD(JFLD,MYPROC)
  IF( .NOT.LLFIRST ) THEN
    CALL GRIB_RELEASE(IGRIB_IN)
  ENDIF
  CALL GRIB_NEW_FROM_MESSAGE(IGRIB_IN,IRECBUF(NGSTA(JFLD):NGSTA(JFLD+1)-1),STATUS=IRET)
  IF(IRET /= GRIB_SUCCESS ) THEN
    write(*,*) ' end of buffer'
    write(*,*) ' iret=',iret
    !      EXIT DECODE
  ENDIF
  IF( LLFIRST ) THEN
      ! we are assuming that per file we have the same date/time/step --> this enters the netcdf
    CALL GRIB_GET(IGRIB_IN,'dataDate',idate)
    CALL GRIB_GET(IGRIB_IN,'dataTime',ITIME)
    ITIME=ITIME/100
    CALL GRIB_GET(IGRIB_IN,'stepType',CSTEPTYPE) ! instant
    CALL GRIB_GET(IGRIB_IN,'stepRange',ISTEP) ! typically fc step
    CALL GRIB_GET(IGRIB_IN,'startStep',ISTARTSTEP) ! same as above if instant
    CALL GRIB_GET(IGRIB_IN,'endStep',IENDSTEP) ! same as above if instant
  ENDIF
  LLFIRST=.FALSE.

  ! PRESENCE OF MISSING VALUES IN INPUT FIELD
  ibitmap=0
  CALL GRIB_GET(IGRIB_IN,'bitmapPresent',ibitmap)
  IF (ibitmap == 1) CALL GRIB_GET(IGRIB_IN,'missingValue',ZINDEF)
  
  ! some checks
  CALL GRIB_GET(IGRIB_IN,'typeOfGrid',CGRIDTYPE) !reduced_gg/sh
  CALL GRIB_GET(IGRIB_IN,'typeOfLevel',CLEVTYPE) ! hybrid/depthBelowLandLayer/surface
  CALL GRIB_GET(IGRIB_IN,'paramId',iparam) ! grib code
  CALL GRIB_GET(IGRIB_IN,'level',ilev) ! 0/1/2/7/128 ... 
  CALL GRIB_GET_SIZE(IGRIB_IN,'values',ISIZE)
  write(*,*) 'processor= ',MYPROC, 'number ', IFLDS, ' owning field= ',iparam
  
  IF ( TRIM(CGRIDTYPE) == 'sh' ) THEN
    if( .not.(spectral%is_null()) ) then
      fields = spset%field(IFLDS)
      call fields%data(gribsdata)
      CALL GRIB_GET(IGRIB_IN,'values',gribsdata(1:ISIZE))
      metadata = fields%metadata()
      LLTYPE(IFLDS,MYPROC) = .TRUE.
    endif
  ELSE
    if( .not.(gridpoints%is_null()) ) then
      fieldg = gpset%field(IFLDS)
      call fieldg%data(fieldgdata)
      CALL GRIB_GET(IGRIB_IN,'values',fieldgdata(1:ISIZE))
      metadata = fieldg%metadata()
    endif
  ENDIF
! does not work ???
!  call metadata%set('paramId',IPARAM)
!  call metadata%set('level',ILEV)
  IPAR(IFLDS,MYPROC) = IPARAM
  ILEVEL(IFLDS,MYPROC) = ILEV

  ! provide grib_info to other processors
  IASEND(1) = IDATE
  IASEND(2) = ITIME
  IASEND(3) = ISTEP
  
ENDDO DECODE

IF( NPROC > 1 ) THEN
  CALL MPL_BARRIER()
ENDIF

LLSPECTRAL(:) = .FALSE.
ILPARAM(:) = 0
ILLEV(:) = 0
IF( NPROC > 1 ) THEN
  ITAG= 543210
  CALL MPL_BROADCAST(IASEND(:),KROOT=IOROOT(1),KTAG=ITAG, CDSTRING='GRIB_INFO1:') 
  DO J=1,JMAX
    IO=IOPROC(J)
    CALL MPL_BROADCAST(LLTYPE(:,IO),KROOT=IO,KTAG=ITAG+J, CDSTRING='GRIB_INFO2:')
    CALL MPL_BROADCAST(IPAR(:,IO),KROOT=IO,KTAG=ITAG+100*J, CDSTRING='GRIB_INFO2:')
    CALL MPL_BROADCAST(ILEVEL(:,IO),KROOT=IO,KTAG=ITAG+1000*J, CDSTRING='GRIB_INFO2:')
    DO JFLD=1,NFIELDS
      IF( IPAR(JFLD,IO) /= 0 ) ILPARAM(JFLD) = IPAR(JFLD,IO)
      IF( ILEVEL(JFLD,IO) /= 0 ) ILLEV(JFLD) = ILEVEL(JFLD,IO)
      IF( LLTYPE(JFLD,IO) ) THEN
        LLSPECTRAL(JFLD) =  .TRUE.
      ENDIF
    ENDDO
  ENDDO
ELSE
  LLSPECTRAL(:) = LLTYPE(:,1)
  ILPARAM(:) = IPAR(:,1)
  ILLEV(:) = ILEVEL(:,1)
ENDIF
INFO%IDATE = IASEND(1)
INFO%ITIME = IASEND(2)
INFO%ISTEP = IASEND(3)

IF( ANY(ITEST .EQ. IOPROC) ) THEN
  write(*,*) MYPROC, ' finished decoding'
ENDIF

if( .not.(gridpoints%is_null()) ) then

  DO IFLDS=1,NFIELDS
    IO = IOROOT(IFLDS)
    IPARAM = ILPARAM(IFLDS)
    ILEV = ILLEV(IFLDS)
    write(fieldname,'(I0)') IFLDS
    
    IF( .NOT.LLSPECTRAL(IFLDS) ) THEN
      field  = gridpoints%create_field(fieldname,atlas_real(JPRB))
      fieldg = gpset%field(IFLDS)
      call gridpoints%scatter(fieldg, field)
      metadata = field%metadata()
      call metadata%set('paramId',IPARAM)
      call metadata%set('level',ILEV)
      call gpfields%add( field )
    ENDIF

    ! special parameters
    IF ((IPARAM == 34).OR. (IPARAM == 31)) THEN
      call field%data(locdata)
      itot = field%size()
      DO j=1,itot
        IF((iparam == 34).and.(locdata(j) == ZINDEF)) locdata(j)=280.0
        IF((iparam == 31).and.(locdata(j) == ZINDEF)) locdata(j)=0.0
      ENDDO
    ENDIF
  ENDDO

  write(msg,'(A)')  " gridpoint scatter finished "
  call log%info(msg)
!  write(msg,'(A)') " gridpoint scatter finished "; call log%info(msg)

endif

if( .not.(spectral%is_null()) ) then

  DO J=1,JMAX
    IO=IOPROC(J)
    IMAXFLDS = IMAXF(J)
    write(fieldname,'(I0)') IMAXFLDS+J
    fields_local  = spectral%create_field(fieldname,atlas_real(JPRB), IMAXFLDS)
    fields = spectral%create_field(fieldname,atlas_real(JPRB), IMAXFLDS, global=.true.,owner= IO-1 )
    IF( MYPROC == IO ) THEN
      DO JFLD=1, IMAXFLDS
        IFLDS = IFIELD(JFLD,IO)
        IF( LLSPECTRAL(IFLDS) ) THEN
          call fields%data(fieldsdata) 
          fields_input = spset%field(IFLDS)
          call fields_input%data(zdata)
          fieldsdata(JFLD,:) = zdata(:)
        ENDIF
      ENDDO
    ENDIF
    call spectral%scatter(fields,fields_local)
    
    DO JFLD=1, IMAXFLDS
      IFLDS = IFIELD(JFLD,IO)
      
      IPARAM = ILPARAM(IFLDS)
      ILEV = ILLEV(IFLDS)
      write(fieldname,'(I0)') IFLDS
      IF( LLSPECTRAL(IFLDS) ) THEN
        field  = spectral%create_field(fieldname,atlas_real(JPRB))
        call fields_local%data(fieldsdata)
        call spfields%add( field )
        call field%data(locdata)
        locdata(:) = fieldsdata(JFLD,:)
        
        metadata = field%metadata()
        call metadata%set('paramId',IPARAM)
        call metadata%set('level',ILEV)
      ENDIF
      
    ENDDO
  ENDDO

  write(msg,'(A)') " spectral scatter finished "; call log%info(msg)

endif

IF( NPROC > 1 ) THEN
  CALL MPL_BARRIER()
ENDIF

IF( ALLOCATED(IMAXF) ) DEALLOCATE(IMAXF)
IF( ALLOCATED(IFIELD) ) DEALLOCATE(IFIELD)
IF( ALLOCATED(LLTYPE)  ) DEALLOCATE(LLTYPE)
IF( ALLOCATED(IPAR)  ) DEALLOCATE(IPAR)
IF( ALLOCATED(ILPARAM)  ) DEALLOCATE(ILPARAM)
IF( ALLOCATED(ILEVEL) ) DEALLOCATE(ILEVEL)
IF( ALLOCATED(ILLEV) ) DEALLOCATE(ILLEV)
IF( ALLOCATED(LLSPECTRAL) )  DEALLOCATE(LLSPECTRAL)
IF( ALLOCATED(IRECBUF) ) DEALLOCATE(IRECBUF)
IF( ALLOCATED(NGSTA) ) DEALLOCATE(NGSTA)
IF( ALLOCATED(IOROOT) )  DEALLOCATE(IOROOT)
IF( ALLOCATED(IOTAG) ) DEALLOCATE(IOTAG)
IF( ALLOCATED(IOPROC) )  DEALLOCATE(IOPROC)
IF( ALLOCATED(ITEST) )  DEALLOCATE(ITEST)
IF( ALLOCATED(IGRIBUF) ) DEALLOCATE(IGRIBUF)
IF( ALLOCATED(ISENDREQ) ) DEALLOCATE(ISENDREQ)
IF( ALLOCATED(IASEND) ) DEALLOCATE(IASEND)
IF( ALLOCATED(IOFFSEND) ) DEALLOCATE(IOFFSEND)
IF( ALLOCATED(ILENSEND) ) DEALLOCATE(ILENSEND)
IF( ALLOCATED(IBYTES) ) DEALLOCATE(IBYTES)

!     3. TIDYING UP
IF( MYPROC == IOMASTER ) THEN
  CALL GRIB_CLOSE_FILE(IFI)
ENDIF



END SUBROUTINE READ_GRIB_SCM

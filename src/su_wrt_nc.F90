SUBROUTINE SU_WRT_NC (myproc,PVAH,PVBH,dataid,inum,incid,klev,nstep)

use yomvar
use fckit_module, only: log => fckit_log

! Setup of relevant quantities for the netcdf files

!  PVAH        I  Vertical coordinate A-table
!  PVBH        I  Vertical coordinate B-table
!  dataid      I  Text identiying the dataset
!  inum        I  station number
!  incid       O  Logical unit number of netcdf file
! klev         I Number of vertical levels

implicit none

#include "netcdf.inc"

INTEGER(KIND=JPIM), intent(in) :: myproc
REAL(KIND=JPRB),    intent(in) :: PVAH(0:KLEV), PVBH(0:KLEV)
character(len=*),   intent(in) :: dataid
INTEGER(KIND=JPIM), intent(out):: incid
INTEGER(KIND=JPIM), intent(in) :: inum
INTEGER(KIND=JPIM), intent(in) :: klev
INTEGER(KIND=JPIM), intent(in) :: nstep

REAL*4    :: zv(0:klev)            ! for netcdf single precision
!REAL(KIND=JPRB)            :: zv(0:klev)
INTEGER(KIND=JPIM) i, &
 & nlevdid, nlevp1did, nlevsdid, ntimdid, nvarid,  &
 &  idimid2(2), idimid3(2), idimid4(2), idimid5(2), &
 & ilev(klev), ilevp1(klev+1), ilevs(ncss), istatus, iaccur
logical   :: file_exist
character (len=50) :: title
! character (len=51) :: nc_name
character (len=40) :: nc_name

INTEGER(KIND=JPIM), PARAMETER :: JPKD=KIND(zv)

CHARACTER*127 msg

!-----------------------------------------------------------------------

!        1.    SETUP LOCAL VARIABLES
!              ---------------------

!        output accuracy
!iaccur=NF_FLOAT
iaccur=NF_DOUBLE

!        2.    open NetCDF file.
!              -----------------
write(msg,'(A,I0)') "NSTEP = ",NSTEP; call log%debug(msg)
write(nc_name,"(A,I5.5,A,I5.5,A,I5.5,A)") 'scm_in_proc_', myproc, '_pt_', inum, '_step_', NSTEP, '.nc'

inquire ( file=nc_name, exist=file_exist )

if ( file_exist ) then
  istatus = NF_OPEN (nc_name, nf_write, incid)       !open old file
  call handle_err_nc(istatus)
  write(msg,'(A)') 'scm_in.nc exists - no setup'; call log%debug(msg)
else
  
  istatus = NF_CREATE (nc_name, nf_clobber, incid)     !create new file
  call handle_err_nc(istatus)

  write(msg,'(A,A,A,I0)') 'NETCDF-FILE ', nc_name, ' OPENED ON UNIT ', incid; call log%debug(msg)

!        3.    meta data set up.
!              -----------------


!        3.1   create and write global data.

!        title
  title = ' SCM input from IFS: ' // trim(dataid)
  istatus = NF_PUT_ATT_TEXT (incid, NF_GLOBAL, 'title',  50, title)
  call handle_err_nc(istatus)

!        data identification
  istatus = NF_PUT_ATT_TEXT (incid, NF_GLOBAL, 'dataID', 30, dataid)
  call handle_err_nc(istatus)

!        number of levels (atmosphere, soil)
!istatus = NF_PUT_ATT_INT  (incid, NF_GLOBAL, 'nlev_atm',  NF_INT, 1, klev)
!call handle_err_nc(istatus)
!istatus = NF_PUT_ATT_INT  (incid, NF_GLOBAL, 'nlev_soil', NF_INT, 1, ncss)
!call handle_err_nc(istatus)

!        parameters of vertical coordinate
  zv=REAL(pvah,JPKD)
  istatus = NF_PUT_ATT_REAL (incid, NF_GLOBAL, 'coor_par_a', &
    & iaccur, klev+1, zv(0:klev))
  call handle_err_nc(istatus)
  zv=REAL(pvbh,JPKD)
  istatus = NF_PUT_ATT_REAL (incid, NF_GLOBAL, 'coor_par_b', &
    & iaccur, klev+1, zv(0:klev))
  call handle_err_nc(istatus)

!        create dimensions
  istatus = NF_DEF_DIM (incid, 'nlev',    klev,         nlevdid)   !atmosphere
  call handle_err_nc(istatus)
  istatus = NF_DEF_DIM (incid, 'nlevp1',  klev+1,       nlevp1did) !atmosphere + 1
  call handle_err_nc(istatus)
  istatus = NF_DEF_DIM (incid, 'nlevs',   ncss,          nlevsdid)  !land/sea-ice
  call handle_err_nc(istatus)
  istatus = NF_DEF_DIM (incid, 'time',    NF_UNLIMITED,  ntimdid)   !time
  call handle_err_nc(istatus)
!istatus = NF_DEF_DIM (incid, 'timestp', NF_UNLIMITED, ntimdid)   !time steps
!call handle_err_nc(istatus)

!        3.2   create dimensional variables.

!        atmospheric model levels
  call ncdf_varsetup1c (incid, NF_INT, 1, nlevdid, &
    & 'nlev',   'Atmospheric Model Levels',      'count')
!        atmospheric model levels + 1
  call ncdf_varsetup1c (incid, NF_INT, 1, nlevp1did, &
    & 'nlevp1', 'Atmospheric Model Half Levels', 'count')
!        soil/sea-ice model levels
  call ncdf_varsetup1c (incid, NF_INT, 1, nlevsdid, &
    & 'nlevs',  'Soil/Sea-Ice Model Levels',     'count')
!        time
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    & 'time',   'Time',                          'seconds')
!        timestep
!call ncdf_varsetup1c (incid, NF_INT, 1, ntimdid, &
!   'timestp', 'Model Time Step',           'count')

!        3.3   create variables.

  idimid2(1) = nlevdid
  idimid2(2) = ntimdid
  idimid3(1) = nlevp1did
  idimid3(2) = ntimdid
  idimid4(1) = nlevsdid
  idimid4(2) = ntimdid

  call ncdf_varsetup1c (incid, NF_INT, 1, ntimdid, &
    & 'date',  'Date',      'yyyymmdd')
  call ncdf_varsetup1c (incid, NF_INT, 1, ntimdid, &
    & 'hour',  'Hour',      's')
  call ncdf_varsetup1c (incid, NF_INT, 1, ntimdid, &
    & 'second','Second',    's')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    & 'lat',   'Latitude',  'deg N')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    & 'lon',   'Longitude', 'deg E')

  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'pressure_f',        'Pressure - full levels',         'Pa')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid3, &
    'pressure_h',        'Pressure - half levels',         'Pa')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'height_f',          'Height - full levels',           'm')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid3, &
    'height_h',          'Height - half levels',           'm')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'ps'              ,'Surface Pressure'             ,'Pa')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'u', 'U Wind', 'm/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'v', 'V Wind', 'm/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    't', 'Temperature', 'K')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'q', 'Water Vapor Mixing Ratio', 'kg/kg')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'ql', 'Liquid Water Mixing Ratio', '1')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'qi', 'Ice Water Mixing Ratio', '1')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'qr', 'Rain', '1')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'qsn', 'Snow', '1')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'cloud_fraction', 'Cloud Fraction', '1')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'pot_temperature',   'Potential Temperature',          'K')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'pot_temp_e',        'Equivalent Potential Temperature','K')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'dry_st_energy',     'Dry Static Energy',              'J/kg')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'moist_st_energy',   'Moist Static Energy',            'J/kg')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'relative_humidity', 'Relative Humidity',              '1')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'q_sat',             'Saturation Specific Humidity',   'kg/kg')

  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'ug', 'Geostrophic U Wind', 'm/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'vg', 'Geostrophic V Wind', 'm/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'tadv', 'Advective T Tendency', 'K/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'qadv', 'Advective Q Tendency', 'kg/kg/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'cladv', 'Advective cloud liquid water Tendency', 'kg/kg/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'ciadv', 'Advective cloud ice water Tendency', 'kg/kg/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'ccadv', 'Advective cloud cover Tendency', 'kg/kg/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'csadv', 'Advective snow water Tendency', 'kg/kg/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'cradv', 'Advective rain water Tendency', 'kg/kg/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'uadv', 'Advective U Tendency', 'm/s^2')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'vadv', 'Advective V Tendency', 'm/s^2')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid2, &
    'omega', 'Vertical Pressure Velocity', 'Pa/s')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid3, &
    'etadotdpdeta', 'Covariant Vert. Vel.', 'Pa/s')

  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'mom_rough'       ,'Momentum Roughness Length'    ,'m')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'heat_rough'      ,'Heat Roughness Length'        ,'m')

  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'high_veg_type'   ,'High Vegetation Type'         ,'1')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'low_veg_type'    ,'Low Vegetation Type'          ,'1')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'high_veg_cover'  ,'High Vegetation Cover'        ,'1')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'low_veg_cover'   ,'Low Vegetation Cover'         ,'1')

  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    't_skin'          ,'Skin Temperature'             ,'K')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'q_skin'          ,'Skin Reservoir Content'       ,'m of water')


  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'sfc_sens_flx'    ,'Surface Sensible Heat Flux'   ,'W/m^2 s')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'sfc_lat_flx'     ,'Surface Latent Heat Flux'     ,'W/m^2 s')

  call ncdf_varsetup1c (incid, iaccur, 2, idimid4, &
    't_soil'          ,'Soil Temperature'             ,'K')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid4, &
    'q_soil'          ,'Soil Moisture'                ,'m of water')

  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'lsm'             ,'Land-Sea Mask'                ,'-')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'sea_ice_frct'    ,'Sea Ice Fraction'             ,'1')
  call ncdf_varsetup1c (incid, iaccur, 2, idimid4, &
    't_sea_ice'       ,'Sea Ice Temperature'          ,'K')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'open_sst'        ,'Open SST'                     ,'K')

  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'snow'            ,'Snow Depth'                   ,'m')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    't_snow'          ,'Snow Temperature'            ,'K')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'albedo_snow'     ,'Snow Albedo'                 ,'-')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
    'density_snow'    ,'Snow Density'                ,'kg/m^3')

  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'orog', 'Orography',      'm')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'sdfor','Orography - SD form drag', '')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'sdor', 'Orography - SD', '')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'isor', 'Orography - IS', '')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'anor', 'Orography - AN', '')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'slor', 'Orography - SL', '')

  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'aluvp' , 'Albedo UV direct'      , '1')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'aluvd' , 'Albedo UV diffuse'     , '1')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'alnip' , 'Albedo near IR direct' , '1')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'alnid' , 'Albedo near IR diffuse', '1')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'albedo', 'Background Albedo'     , '1')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'soty'  , 'Soil Type'                      , '1-6')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'lail'  , 'Leaf Area Index Low Vegitation' , 'm2/m2')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'laih'  , 'Leaf Area Index High Vegitation', 'm2/m2')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'cl'    , 'Lake fraction', '1')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'dl'    , 'Lake depth', 'm')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'mlt'    , 'Lake mix-layer temperature', 'K')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'mld'    , 'Lake mix-layer depth', 'm')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'blt'    , 'Lake bottom temperature', 'K')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'tlt'    , 'Lake total layer temperature', 'K')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'shf'    , 'Lake shape factor', '1')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'ict'    , 'Lake ice temperature', 'K')
  call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, 'icd'    , 'Lake ice depth', 'm')

!   call ncdf_varsetup1c (incid, iaccur, 1, ntimdid, &
!    'emiss'           ,'Background LW Emissivity'     ,'1')


!        4.    write model level data
!              ----------------------

!        4.1   return to data mode
  istatus = NF_ENDDEF(incid)
  call handle_err_nc(istatus)

!        4.2   atmospheric model levels
  DO i=1,klev
    ilev(i)=i
  ENDDO
  istatus = NF_INQ_VARID   (incid, 'nlev', nvarid)
  call handle_err_nc(istatus)
  istatus = NF_PUT_VAR_INT (incid, nvarid, ilev)
  call handle_err_nc(istatus)

!        4.3   atmospheric model half levels
  DO i=1,klev+1
    ilevp1(i)=i
  ENDDO
  istatus = NF_INQ_VARID   (incid, 'nlevp1', nvarid)
  call handle_err_nc(istatus)
  istatus = NF_PUT_VAR_INT (incid, nvarid, ilevp1)
  call handle_err_nc(istatus)

!        4.4   soil/sea-ice model levels
  DO i=1,ncss
    ilevs(i)=i
  ENDDO
  istatus = NF_INQ_VARID   (incid, 'nlevs', nvarid)
  call handle_err_nc(istatus)
  istatus = NF_PUT_VAR_INT (incid, nvarid, ilevs)
  call handle_err_nc(istatus)

!        4.5   parameters of vertical coordinate
!istart2(1) = 1          ! 2-d variables - dim 1: starting index
!icount2(1) = klev      !      -"-               written indices
!istart2(2) = 1          ! 2-d variables - dim 2: starting index
!icount2(2) = 1          !      -"-               written indices
!call ncdf_varwrite1c (incid, 1, istart1,icount1, 'coor_par_a', pvah)
!call ncdf_varwrite1c (incid, 1, istart1,icount1, 'coor_par_b', pvbh)

!istatus = NF_PUT_ATT_REAL (incid, NF_GLOBAL, 'coor_par_a', NF_FLOAT, klev+1, &
!          pvah)
!call handle_err_nc(istatus)
!istatus = NF_PUT_ATT_REAL (incid, NF_GLOBAL, 'coor_par_b', NF_FLOAT, klev+1, &
!          pvbh)
!call handle_err_nc(istatus)

endif          ! if ( file_exist == .false. ) then  else  endif

RETURN
END SUBROUTINE SU_WRT_NC

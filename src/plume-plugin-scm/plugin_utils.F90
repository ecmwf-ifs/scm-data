module plugin_utils_mod

implicit none

! All Surf fields
integer, parameter :: n_fields_srf = 49
character(len=16)  :: field_names_srf(n_fields_srf) = [character(len=16) :: "stl1", "stl2", "stl3", "stl4", "swvl1", "swvl2", "swvl3", "swvl4", &
  "sd", "src", "skt", "ci", "lmlt", "lmld", "lblt", "ltlt", "lshf", "lict", "licd", "tsn", "asn", "rsn", "sst", &
  "istl1", "istl2", "istl3", "istl4", "lsm", "sr", "al", "aluvp", "alnip", "aluvd", "alnid", "lai_lv", "lai_hv", "sdfor", "slt", "sdor", &
  "isor", "anor", "slor", "lsrh", "cvh", "cvl", "tvh", "tvl", "cl", "dl"]


! All Cloud fields
integer, parameter :: n_fields_cld = 6
character(len=16)  :: field_names_cld(n_fields_cld) = [character(len=16) :: "cc", "ciwc", "clwc", "crwc", "cswc", "q"]


! Spectral fields
integer, parameter :: n_fields_spc = 6
character(len=16)  :: field_names_spc(n_fields_spc) = [character(len=16) :: "d", "lnsp", "t", "vo", "z", "w"]
! -------------------------------------------------------------------------> 155, 152, 130, 138, 129, 135 

! Other parameters
integer, parameter :: n_fields_oth = 5
character(len=16)  :: field_names_oth(n_fields_oth) = [character(len=16) :: "u", "v", "100u", "100v", "tcw"]


! All fields
integer, parameter :: n_fields = n_fields_srf + n_fields_cld + n_fields_spc + n_fields_oth
character(len=16)  :: field_names(n_fields) = [character(len=16) :: "stl1", "stl2", "stl3", "stl4", "swvl1", "swvl2", "swvl3", "swvl4", &
  "sd", "src", "skt", "ci", "lmlt", "lmld", "lblt", "ltlt", "lshf", "lict", "licd", "tsn", "asn", "rsn", "sst", &
  "istl1", "istl2", "istl3", "istl4", "lsm", "sr", "al", "aluvp", "alnip", "aluvd", "alnid", "lai_lv", "lai_hv", "sdfor", "slt", "sdor", &
  "isor", "anor", "slor", "lsrh", "cvh", "cvl", "tvh", "tvl", "cl", "dl", "cc", "ciwc", "clwc", "crwc", "cswc", &
  "d", "lnsp", "q", "t", "vo", "z", "w", "u", "v", "100u", "100v", "tcw"]

contains


function param_name2idx(name)
    integer :: param_name2idx
    character(*), intent(in) :: name
    do param_name2idx=1,size(field_names)
        if (trim(field_names(param_name2idx)) == name) then
            return
        endif
    enddo
end function


function param_name2id(name) result(id)
    character(len=*), intent(in) :: name
    integer :: id

    if (trim(name) == "stl1") then
        id = 139
    else if (trim(name) == "stl2") then
        id = 170
    else if (trim(name) == "stl3") then
        id = 183
    else if (trim(name) == "stl4") then
        id = 236
    else if (trim(name) == "swvl1") then
        id = 39
    else if (trim(name) == "swvl2") then
        id = 40
    else if (trim(name) == "swvl3") then
        id = 41
    else if (trim(name) == "swvl4") then
        id = 42
    else if (trim(name) == "sd") then
        id = 141
    else if (trim(name) == "src") then
        id = 198
    else if (trim(name) == "skt") then
        id = 235
    else if (trim(name) == "ci") then
        id = 31
    else if (trim(name) == "lmlt") then
        id = 228008
    else if (trim(name) == "lmld") then
        id = 228009
    else if (trim(name) == "lblt") then
        id = 228010
    else if (trim(name) == "ltlt") then
        id = 228011
    else if (trim(name) == "lshf") then
        id = 228012
    else if (trim(name) == "lict") then
        id = 228013
    else if (trim(name) == "licd") then
        id = 228014
    else if (trim(name) == "tsn") then
        id = 238
    else if (trim(name) == "asn") then
        id = 32
    else if (trim(name) == "rsn") then
        id = 33
    else if (trim(name) == "sst") then
        id = 34
    else if (trim(name) == "istl1") then
        id = 35
    else if (trim(name) == "istl2") then
        id = 36
    else if (trim(name) == "istl3") then
        id = 37
    else if (trim(name) == "istl4") then
        id = 38
    else if (trim(name) == "lsm") then
        id = 172
    else if (trim(name) == "sr") then
        id = 173
    else if (trim(name) == "al") then
        id = 174
    else if (trim(name) == "aluvp") then
        id = 15
    else if (trim(name) == "alnip") then
        id = 17
    else if (trim(name) == "aluvd") then
        id = 16
    else if (trim(name) == "alnid") then
        id = 18
    else if (trim(name) == "lai_lv") then
        id = 66
    else if (trim(name) == "lai_hv") then
        id = 67
    else if (trim(name) == "sdfor") then
        id = 74
    else if (trim(name) == "slt") then
        id = 43
    else if (trim(name) == "sdor") then
        id = 160
    else if (trim(name) == "isor") then
        id = 161
    else if (trim(name) == "anor") then
        id = 162
    else if (trim(name) == "slor") then
        id = 163
    else if (trim(name) == "lsrh") then
        id = 234
    else if (trim(name) == "cvh") then
        id = 28
    else if (trim(name) == "cvl") then
        id = 27
    else if (trim(name) == "tvh") then
        id = 30
    else if (trim(name) == "tvl") then
        id = 29
    else if (trim(name) == "cl") then
        id = 26
    else if (trim(name) == "dl") then
        id = 228007
    else if (trim(name) == "cc") then
        id = 248
    else if (trim(name) == "ciwc") then
        id = 247
    else if (trim(name) == "clwc") then
        id = 246
    else if (trim(name) == "crwc") then
        id = 75
    else if (trim(name) == "cswc") then
        id = 76
    else if (trim(name) == "d") then
        id = 155
    else if (trim(name) == "lnsp") then
        id = 152
    else if (trim(name) == "q") then
        id = 133
    else if (trim(name) == "t") then
        id = 130
    else if (trim(name) == "vo") then
        id = 138
    else if (trim(name) == "z") then
        id = 129
    else if (trim(name) == "w") then
        id = 135        
    else if (trim(name) == "u") then
        id = 131
    else if (trim(name) == "v") then
        id = 132
    else if (trim(name) == "100u") then
        id = 228246
    else if (trim(name) == "100v") then
        id = 228247
    else if (trim(name) == "tcw") then
        id = 136
    else 
        id = -999
    endif

    end function param_name2id


! NOTE: This routine is for TESTING PURPOSES ONLY - used to read vertical tables from the test namelist
subroutine get_vertical_tables_from_namelist(vtable_testing_namelist, NLEV, PVAH, PVBH)

    use yomvar

    implicit none

    character(256) :: vtable_testing_namelist
        
    INTEGER, PARAMETER :: JMAXPTS = 100
    INTEGER, PARAMETER :: JMAXLEV = 200

    LOGICAL :: larea, lprognostic
    INTEGER(KIND=JPIM) :: KLOCMAX, NSMAX, NSTEP

    character(len=30) :: dataid, cgrid
    REAL(KIND=JPRB), allocatable :: PLAT(:), PLON(:)
    REAL(KIND=JPRB) :: DELTA
    REAL(KIND=JPRB) :: LAT(JMAXPTS), LON(JMAXPTS)
    REAL(KIND=JPRB) :: DVALH(0:JMAXLEV), DVBH(0:JMAXLEV)  

    REAL(KIND=JPRB) :: LATN, LATS, LONW, LONE, TSTEP    
    REAL(KIND=JPRB), allocatable, intent(out) :: PVAH(:), PVBH(:)
    INTEGER(KIND=JPIM), intent(out) :: NLEV
    
    NAMELIST /NAMUS/ LAT, LON, LATN, LATS, LONW, LONE, LPROGNOSTIC, &
     &               DATAID,TSTEP, DELTA, NSTEP, NLEV, NSMAX, CGRID, DVALH, DVBH

    NLEV=-1

    ! Read the namelist file
    OPEN(51,FILE=vtable_testing_namelist)    
    READ(51,NAMUS)
    
    if( NLEV > -1 ) THEN
      allocate(pvah(0:nlev))
      allocate(pvbh(0:nlev))
      pvah(0:nlev) = DVALH(0:nlev)
      pvbh(0:nlev) = DVBH(0:nlev)
    endif
    
end subroutine get_vertical_tables_from_namelist


end module plugin_utils_mod
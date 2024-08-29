INTERFACE
subroutine calcgeost( &
 &   klev, pah, pbh, pslat  &
 & , pt    , pq  , psp                     &
 & , ptl   , pql , pspl  , pzl                    &
 & , ptm   , pqm , pspm  , pzm                    &
 & , pug   , pvg)

implicit none

INTEGER, PARAMETER :: JPIM = SELECTED_INT_KIND(9)

#ifdef WITH_SCM_SINGLE_PRECISION
INTEGER, PARAMETER :: JPRB = SELECTED_REAL_KIND(6,37)   ! SINGLE_PRECISION
#else
INTEGER, PARAMETER :: JPRB = SELECTED_REAL_KIND(13,300) ! DOUBLE_PRECISION
#endif

! Computes the geostrophic wind for a set of levels

REAL(KIND=JPRB), intent(in) :: &
 &   pt(klev) , pq(klev) , psp, &
 &   ptl(klev), pql(klev), pspl, pzl, &
 &   ptm(klev), pqm(klev), pspm, pzm, &
 &   pslat
REAL(KIND=JPRB), intent(in) :: pah(0:klev) , pbh(0:klev)
REAL(KIND=JPRB), intent(out) :: pug(klev), pvg(klev)
INTEGER(KIND=JPIM), intent(in) :: klev

end subroutine calcgeost
END INTERFACE

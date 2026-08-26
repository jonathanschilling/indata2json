module nzl
implicit none

contains

FUNCTION NonZeroLen(array, n)
  USE stel_kinds, ONLY: dp
  use stel_constants, only: zero
  IMPLICIT NONE

  integer :: NonZeroLen
  INTEGER, INTENT(IN)      :: n
  REAL(dp), INTENT(IN)  :: array(n)
  INTEGER :: k

  DO k = n, 1, -1
    IF (array(k) .NE. zero) EXIT
  END DO

  NonZeroLen = k

END ! FUNCTION NonZeroLen

FUNCTION NonNegLen(array, n)
  ! Like NonZeroLen, but scans for the last entry that is >= 0 rather than
  ! != 0. VMEC marks unused spline knots in *_aux_s with a sentinel of -1
  ! (see vmec_input.f defaults), so this is the correct predicate for aux_s
  ! arrays: a knot at exactly s = 0.0 in the last used slot must still count
  ! as set, which NonZeroLen would mistruncate.
  USE stel_kinds, ONLY: dp
  use stel_constants, only: zero
  IMPLICIT NONE

  integer :: NonNegLen
  INTEGER, INTENT(IN)      :: n
  REAL(dp), INTENT(IN)  :: array(n)
  INTEGER :: k

  DO k = n, 1, -1
    IF (array(k) .GE. zero) EXIT
  END DO

  NonNegLen = k

END ! FUNCTION NonNegLen

end ! module nzl

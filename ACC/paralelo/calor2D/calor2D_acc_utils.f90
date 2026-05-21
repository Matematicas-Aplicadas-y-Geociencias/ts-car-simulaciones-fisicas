#ifndef NX
#define NX 300
#endif
#ifndef NY
#define NY 150
#endif

module calor2D_acc_utils
  implicit none

  integer, parameter, public :: acc_nx = NX, acc_ny = NY

contains

  ! Column-major: (ii,jj) -> ii + nx*(jj-1), barrido en y
  integer function indicex(ii, jj)
    !$acc routine seq
    integer, intent(in) :: ii, jj

    indicex = ii + acc_nx * (jj - 1)
  end function indicex

  ! Row-major por columna: (ii,jj) -> jj + ny*(ii-1), barrido en x
  integer function indicey(ii, jj)
    !$acc routine seq
    integer, intent(in) :: ii, jj

    indicey = jj + acc_ny * (ii - 1)
  end function indicey

  ! Thomas por eliminacion (modifica b y r in situ). !$acc routine seq
  subroutine tri(a, b, c, r, u, n)
    !$acc routine seq
    implicit none
    integer, intent(in) :: n
    double precision, intent(in) :: a(n), c(n)
    double precision, intent(inout) :: b(n), r(n)
    double precision, intent(out) :: u(n)
    integer :: i

    u = 0.d0
    do i = 2, n
       r(i) = r(i) - (a(i) / b(i - 1)) * r(i - 1)
       b(i) = b(i) - (a(i) / b(i - 1)) * c(i - 1)
    end do

    u(n) = r(n) / b(n)
    do i = n - 1, 1, -1
       u(i) = (r(i) - c(i) * u(i + 1)) / b(i)
    end do
  end subroutine tri

end module calor2D_acc_utils

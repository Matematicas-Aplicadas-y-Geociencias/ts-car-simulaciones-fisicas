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

  subroutine tri_factor(a, b, c, cp, den, n)
    implicit none
    integer, intent(in) :: n
    double precision, intent(in) :: a(n), b(n), c(n)
    double precision, intent(out) :: cp(n), den(n)
    integer :: i

    den(1) = b(1)
    cp(1) = c(1) / den(1)

    do i = 2, n - 1
       den(i) = b(i) - a(i) * cp(i - 1)
       cp(i) = c(i) / den(i)
    end do

    den(n) = b(n) - a(n) * cp(n - 1)
    cp(n) = 0.d0
  end subroutine tri_factor

  ! Thomas con factorizacion previa; modifica solo el lado derecho local.
  subroutine tri_solve(a, cp, den, r, u, n)
    !$acc routine seq
    implicit none
    integer, intent(in) :: n
    double precision, intent(in) :: a(n), cp(n), den(n)
    double precision, intent(inout) :: r(n)
    double precision, intent(out) :: u(n)
    integer :: i

    r(1) = r(1) / den(1)
    do i = 2, n
       r(i) = (r(i) - a(i) * r(i - 1)) / den(i)
    end do

    u(n) = r(n)
    do i = n - 1, 1, -1
       u(i) = r(i) - cp(i) * u(i + 1)
    end do
  end subroutine tri_solve

  ! Version completa de Thomas, conservada para pruebas de barrido aislado.
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

module calor2D_utils
  implicit none

contains

  subroutine tri_factor(a, b, c, cp, den, nn)
    implicit none
    integer, intent(in) :: nn
    double precision, intent(in)  :: a(nn), b(nn), c(nn)
    double precision, intent(out) :: cp(nn), den(nn)
    integer :: i

    den(1) = b(1)
    cp(1)  = c(1) / den(1)

    do i = 2, nn - 1
       den(i) = b(i) - a(i) * cp(i - 1)
       cp(i)  = c(i) / den(i)
    end do
    den(nn) = b(nn) - a(nn) * cp(nn - 1)
    cp(nn)  = 0.d0
  end subroutine tri_factor

  subroutine tri_solve(a, cp, den, r, u, nn)
    implicit none
    integer, intent(in)    :: nn
    double precision, intent(in)    :: a(nn), cp(nn), den(nn)
    double precision, intent(inout) :: r(nn)
    double precision, intent(out)   :: u(nn)
    integer :: ii

    r(1) = r(1) / den(1)
    do ii = 2, nn
       r(ii) = (r(ii) - a(ii) * r(ii - 1)) / den(ii)
    end do

    u(nn) = r(nn)
    do ii = nn - 1, 1, -1
       u(ii) = r(ii) - cp(ii) * u(ii + 1)
    end do
  end subroutine tri_solve

  subroutine calc_residuo2(tt_new, tt_old, nx, ny, res)
    use omp_lib
    implicit none
    integer, intent(in)           :: nx, ny
    double precision, intent(in)  :: tt_new(nx, ny), tt_old(nx, ny)
    double precision, intent(out) :: res
    integer :: ii, jj

    res = 0.d0
    !$omp parallel do default(none) private(ii, jj) &
    !$omp shared(tt_new, tt_old, nx, ny) reduction(+:res) schedule(static)
    do jj = 1, ny
       do ii = 1, nx
          res = res + (tt_new(ii, jj) - tt_old(ii, jj)) * (tt_new(ii, jj) - tt_old(ii, jj))
       end do
    end do
    !$omp end parallel do
  end subroutine calc_residuo2

end module calor2D_utils

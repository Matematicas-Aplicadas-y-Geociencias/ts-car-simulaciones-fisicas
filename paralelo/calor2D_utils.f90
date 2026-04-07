module calor2D_utils
  implicit none

contains

  !=========================================================
  ! Solver tridiagonal (Thomas)
  ! Modifica b y r in-place
  !=========================================================
  subroutine tri(a, b, c, r, u, n)
    implicit none
    integer, intent(in) :: n
    double precision, intent(in)    :: a(n), c(n)
    double precision, intent(inout) :: b(n), r(n)
    double precision, intent(out)   :: u(n)

    integer :: i
    double precision :: m

    do i = 2, n
       m    = a(i) / b(i-1)
       b(i) = b(i) - m*c(i-1)
       r(i) = r(i) - m*r(i-1)
    end do

    u(n) = r(n) / b(n)

    do i = n-1, 1, -1
       u(i) = (r(i) - c(i)*u(i+1)) / b(i)
    end do
  end subroutine tri

  !=========================================================
  ! Calcula el residuo de convergencia (norma L2 de tt_new - tt_old)
  !=========================================================
  subroutine calc_residuo(tt_new, tt_old, nx, ny, res)
    use omp_lib
    implicit none
    integer, intent(in)           :: nx, ny
    double precision, intent(in)  :: tt_new(nx,ny), tt_old(nx,ny)
    double precision, intent(out) :: res
    integer :: ii, jj

    res = 0.d0
    !$omp parallel do default(none) private(ii,jj) &
    !$omp shared(nx,ny,tt_new,tt_old) reduction(+:res)
    do ii = 1, nx
       do jj = 1, ny
          res = res + (tt_new(ii,jj) - tt_old(ii,jj))**2
       end do
    end do
    !$omp end parallel do

    res = sqrt(res)
  end subroutine calc_residuo

end module calor2D_utils

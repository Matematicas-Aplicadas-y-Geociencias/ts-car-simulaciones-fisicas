module calor2D_acc_utils
  implicit none

contains

  subroutine tri(a, b, c, r, u, n)
    !$acc routine seq
    implicit none
    integer, intent(in) :: n
    double precision, intent(in) :: a(n), c(n)
    double precision, intent(inout) :: b(n), r(n)
    double precision, intent(out) :: u(n)

    integer :: ii
    double precision :: m

    do ii = 2, n
       m = a(ii)/b(ii-1)
       b(ii) = b(ii) - m*c(ii-1)
       r(ii) = r(ii) - m*r(ii-1)
    end do

    u(n) = r(n)/b(n)

    do ii = n-1, 1, -1
       u(ii) = (r(ii) - c(ii)*u(ii+1))/b(ii)
    end do
  end subroutine tri

end module calor2D_acc_utils

program producto_punto_omp
    use omp_lib
    implicit none
    integer :: i, n
    real(8), allocatable :: a(:), b(:)
    real(8) :: prod

    n = 10
    allocate(a(n), b(n))

    !$omp parallel do
    do i = 1, n
        a(i) = i
    end do
    !$omp end parallel do
    b = 3.0d0
    prod = 0.0d0

    !$omp parallel do reduction(+:prod)
    do i = 1, n
        prod = prod + a(i)*b(i)
    end do
    !$omp end parallel do

    print *, "Producto punto =", prod

    deallocate(a, b)
end program
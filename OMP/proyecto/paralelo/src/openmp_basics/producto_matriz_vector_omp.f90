program producto_matriz_vector_omp
    use omp_lib
    implicit none
    integer :: ii, jj, n, m
    real(8), allocatable :: a(:), B(:,:), y(:)
    real(8) :: suma

    n = 10
    m = 12
    allocate(a(n), B(n,m), y(m))

    ! Se crea arreglo a con valores 1, 2, ..., n
    !$omp parallel do
    do ii = 1, n
        a(ii) = ii  ! a = [1, 2, ..., n]
    end do
    !$omp end parallel do
    B = 3.0d0
    y = 0.0d0

    ! Producto matriz-vector: y = B^T * a
    !$omp parallel do private(ii, suma) shared(y)
    do jj = 1, m
        suma = 0.0d0
        do ii = 1, n
            suma = suma + a(ii) * B(ii,jj)
        end do
        y(jj) = suma
    end do
    !$omp end parallel do

    print *, "Vector resultado y =", y

    deallocate(a, B, y)
end program
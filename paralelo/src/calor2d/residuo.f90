subroutine residue(T, nx, ny, dx, dy, residuo)
    ! Calcula el residuo local de la ecuación de Laplace en cada punto interior
    implicit none
    integer, intent(in) :: nx, ny
    double precision, intent(in) :: T(nx,ny), dx, dy
    double precision, intent(out) :: residuo(nx,ny)
    integer :: ii, jj
    double precision :: maxres

    residuo(:,:) = 0.d0

    !$omp parallel do private(ii,jj) shared(T,dx,dy,residuo)
    do ii = 2, nx-1
       do jj = 2, ny-1
          residuo(ii,jj) = (T(ii+1,jj) - 2.0d0*T(ii,jj) + T(ii-1,jj)) / dx**2 + &
                           (T(ii,jj+1) - 2.0d0*T(ii,jj) + T(ii,jj-1)) / dy**2
       end do
    end do
    !$omp end parallel do

    ! Imprimir el máximo residuo absoluto para monitoreo
    maxres = 0.d0
    do ii = 2, nx-1
       do jj = 2, ny-1
          if (abs(residuo(ii,jj)) > maxres) maxres = abs(residuo(ii,jj))
       end do
    end do
    write(*,*) 'Residuo máximo local = ', maxres

end subroutine residue
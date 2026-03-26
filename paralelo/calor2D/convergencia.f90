subroutine residuo_temp(tt, nx, ny, lx, ly, residuo)
!
! Subrutina que calcula la convegencia 
! de la ecuación de Laplace en 2D
! basado en la discretización de diferencias
! finitas
use omp_lib
!
implicit none
!
!
integer,          intent(in)  :: nx, ny                                                                   
double precision, intent(in)  :: tt(nx,ny)
double precision, intent(in)  :: lx, ly
double precision, intent(out) :: residuo
!
integer :: ii, jj

double precision :: deltax2, deltay2
!
!
residuo = 0.d0
deltax2 = (lx/nx)**2
deltay2 = (ly/ny)**2
!
! Empezamos el ciclo para la iteración
print*, "Prueba: ", deltax2, deltay2
!$omp parallel do default(none) &
!$omp shared(tt, deltax2, deltay2, nx, ny) &
!$omp private(jj) reduction(+:residuo)
bucle_x: do ii = 2, nx-1
    bucle_y: do jj=2, ny-1
        residuo = residuo + (1/(deltax2))*(tt(ii-1, jj) + tt(ii+1, jj)) +& 
        & (1/(deltay2))*(tt(ii, jj-1) + tt(ii, jj+1)) -& 
        & 2.0*(tt(ii, jj))*(1/deltax2 + 1/deltay2)
    end do bucle_y
end do bucle_x
!$omp end parallel do
!
!
end subroutine residuo_temp
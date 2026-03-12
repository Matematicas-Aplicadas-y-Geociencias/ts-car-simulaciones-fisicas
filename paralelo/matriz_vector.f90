Program producto_matriz_vector
!Importar libreria
use omp_lib
!
implicit none
!Definimos la variables que se van a usar
integer :: ii, jj
integer, parameter:: N = 3
real :: A(N,N) 
real :: v(N) 
real :: producto(N)
producto(:) = 0
!Usamos el reshape para asignar los valores a la matriz order [2, 1]
!Primero llena las filas, si no se usa primero llena las columnas
! 1 2 3
! 4 5 6
! 7 8 9
!
A = reshape([1,2,3,4,5,6,7,8,9],[3,3], order = [2, 1])
v = [1,1,1]
print*, "La matriz que vamos a usar es"
do ii=1, N
   print*, A(ii, :)
end do
!
!
!$omp parallel shared(producto, A, v) 
!$omp do 
do ii=1,N
   do jj=1,N
      producto(ii)=producto(ii)+A(ii,jj)*v(jj)
   end do
end do
!$omp end do
!$omp end parallel
!
print*, "El producto es el siguiente: "
print*, producto(:)
!
end Program  producto_matriz_vector
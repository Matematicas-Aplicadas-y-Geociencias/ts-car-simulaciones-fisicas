Program matriz_vector
use omp_lib
implicit none
integer :: id_thread
integer :: ii, jj
integer, parameter :: N = 2
real :: A(N,N)
real :: v(N)
real :: producto(N)
A = reshape([1,3,2,4],[2,2])
v = [2, -5]
!$omp parallel private(id_thread) shared(producto,A,v)
id_thread = omp_get_thread_num()
!$omp do 
do ii=1,N
   print*, "DEBUG: ", id_thread
   producto(ii)=0
   do jj=1,N
      producto(ii)=producto(ii)+A(ii,jj)*v(jj)
   end do
  print*, "el producto procede como: ", producto(ii), " para la iteracion: ", ii, " desde ",  id_thread
end do
!$omp end do
!$omp end parallel
print*, producto
end Program  matriz_vector

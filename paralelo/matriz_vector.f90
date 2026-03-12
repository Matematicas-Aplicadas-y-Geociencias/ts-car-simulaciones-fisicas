Program matriz_vector 


use omp_lib

implicit none
integer :: suma, id_thread
integer :: ii, jj
integer, parameter :: N = 3

real :: A(N,N)
real :: v(N)
real :: producto(N)

A = reshape([1,3,5,2,4,6,9,8,7],[3,3])
v = [2, -5, 8]

!$omp parallel private(id_thread) shared(producto, A, V)
id_thread = omp_get_thread_num()
!$omp do 
do ii=1,N
print*, "Debug: ", id_thread
   producto(ii)=0
   do jj=1,N
      producto(ii)=producto(ii)+A(ii,jj)*v(jj)
   end do
  print*, "el producto procede como: ", producto(ii), " para la iteracion: ", ii, " desde ", id_thread 
end do
!$omp end do
!$omp end parallel

print*, producto
end Program  matriz_vector

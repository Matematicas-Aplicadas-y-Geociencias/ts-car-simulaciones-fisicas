!
! Programa de ejemplo para uso de open acc
!
program ejemploArreglo
!
use openacc
!
implicit none
!
integer, parameter 		:: dimen = 1000
!
double precision 		:: arreglo(dimen)
!
integer 			:: ii
!
!$acc parallel loop
do ii = 1, dimen
	!
	arreglo(ii) = 2.d0 * ii
	!
end do
!$acc end parallel loop
!
print*, "Elemento 13 de arreglo", arreglo(13)
!
end program ejemploArreglo 

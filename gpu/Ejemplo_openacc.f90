! Ejemplo openacc
program ejemploacc
	use openacc
	
	implicit none
	
	integer, parameter:: dimen = 10000
	double precision :: arreglo(dimen)
	integer :: ii
	!$acc parallel loop
	do ii=1, dimen
	arreglo(ii)=2.d0*ii
	end do
	!$acc end parallel loop
	print*, "Elementos del arreglo", arreglo(dimen) 
	

end program ejemploacc

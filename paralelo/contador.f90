Program contador
!
!
	use omp_lib
	Implicit none
	
	integer::nthreads
	integer::cuenta =0
	
	!$omp parallel private(nthreads) reduction(+:cuenta) 
		 
		nthreads = omp_get_thread_num() 
		!write(*,*) "Thread: ", nthreads
		cuenta = cuenta + nthreads 
	!$omp end parallel 
	write(*,*) "La suma es: ", cuenta
end program contador

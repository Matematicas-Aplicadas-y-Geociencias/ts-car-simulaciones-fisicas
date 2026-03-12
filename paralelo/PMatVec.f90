program prod_mar_vec
    use omp_lib
    implicit none
    integer :: i,j, id_thread
    real:: a(3,3), b(3), r(3)
    
    a(1,1) = 6   
    a(2,1) = -1  
    a(3,1) = -2 
    
    a(1,2) = 2   
    a(2,2) = 4   
    a(3,2) = 9  
    
    a(1,3) = 4   
    a(2,3) = 3   
    a(3,3) = 3   
    
    b(1) = 4
    b(2) = -2
    b(3) = 1
    
    r=0
    
  !$omp parallel private(id_thread) shared(r,a,b)
   id_thread = omp_get_thread_num()
   
  !$omp do
    do i = 1,3
    	do j= 1,3
    	  print*, "DEBUG: ",id_thread, id_thread
    	  r(i)=r(i)+a(i,j)*b(j)
 	end do
    end do
    !$omp end do
 !Cerramos la región paralela 
 !$omp end parallel
 print*, "El producto de a*b es:", r
 end program prod_mar_vec


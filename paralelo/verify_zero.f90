 subroutine verify_zero(t,dx,dy,tol)
 	use omp_lib
 	double precision, intent(in) :: t(:,:)
 	double precision, intent(in) :: tol
 	double precision, intent(in) :: dx,dy
 	double precision :: dx2,dy2 !Se calculan dentro no intent
 	double precision :: error
 	integer :: nx,ny,i,j
 	logical :: exceeded 
 	dx2 = 1.d0/(dx*dx)
  	dy2 = 1.d0/(dy*dy)
  	nx = size(t,1)
  	ny = size(t,2)
  	error=0.d0
    	exceeded = .false.
 	!$omp parallel do default(none) &
 	!$omp shared(dx2,dy2,nx,ny,tol,t,exceeded) &
 	!$omp private(i,j,error)
 		do j=2,ny-1
 			do i=2, nx-1
 			error = (dx2*(t(i-1,j)+t(i+1,j))+ &
 			dy2*(t(i,j-1)+t(i,j+1)) -&
 			2.0d0*(dx2+dy2)*t(i,j))*dx*dy
 			if (abs(error) > tol) then
  			  print *, "La solución no converge: ", i, j
     			  exceeded = .true. 
                        end if
 			end do
 		end do
 		
 	!$omp end parallel do

end subroutine

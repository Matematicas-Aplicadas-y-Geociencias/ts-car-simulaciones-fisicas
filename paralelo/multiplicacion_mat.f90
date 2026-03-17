program Matrixprod
	use omp_lib
	Implicit none
	
	integer, parameter :: N = 6
	integer:: ii, jj, ll, mm, pp
	!Declaracion de una matriz NxN, un vector de tamaño N
	!y un vector resultado
	integer:: M(N,N)
	integer:: V(N)
	integer:: VR(N)
	!Inicialización de los vectores y matriz
	V(:) = 1.d0
	VR(:) = 0.d0
	M(:,:) = 0.d0
	!Inicialización elemento por elemento paralelizando
	!$omp parallel shared(M)
		!$omp parallel do
		do ii = 1, N
			do jj = 1,N
			!if(ii.eq.jj) M(ii,jj) = 1.d0 Matriz para comprobar que funciona el producto
			M(ii,jj) = ii + jj
			end do
		end do
		!$omp end parallel do
	!$omp end parallel
	!Se realiza la operación de multiplicación de matriz por vector

	!$omp parallel do shared (M,V,VR)
	do mm = 1,N
		do ll= 1,N
		VR(mm) = VR(mm) + M(mm,ll)*V(ll)
		end do
	end do
	!$omp end parallel do
	
	
	do ii =1,N
		write(*,*) M(ii,:)
	end do 
	
	do pp =1,N
	write(*,*) V(pp), VR(pp)
	end do
end program Matrixprod
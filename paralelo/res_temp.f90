subroutine res_temp(res_ec_dif, deltax, deltay, tt,nx,ny)
!
!
	use omp_lib
	Implicit none
	!
	!
	double precision, intent(in) :: tt(:,:)
	double precision, intent(in) :: deltax, deltay 
	integer, intent(in) :: nx, ny
	double precision, intent(out) :: res_ec_dif
	!
	integer :: ii,jj
	!
	!
	double precision:: ttt(nx,ny,5) !Este tensor va a contener a los pasos anteriores y posteriores en x y y 
	!
	ttt(:,:,:) = 0.d0 !Inicialización en cero
	!

	do ii = 1, nx
		do jj = 1, ny
		ttt(ii,jj,2) = tt(ii,jj)  !Copia paso actual x y y 
		end do
	end do
	
	do ii = 2, nx-1
		ttt(ii,:,1) = tt(ii-1,:) !Copia paso anterior x
	end do
	
	do ii = 2, nx-1
		ttt(ii,:,3) = tt(ii+1,:) !Copia paso posterior x
	end do
	
	do jj = 2, ny-1
		ttt(:,jj,4) = tt(:,jj-1) !Copia paso anterior y
	end do
	
	do jj = 2, ny-1
		ttt(:,jj,5) = tt(:,jj+1) !Copia paso posterior y
	end do
	!
	!
	!$omp parallel do shared(deltax, deltay,ttt) default(none) private(ny,res_ec_dif,jj) 
     Ec_diferencial_parcial: do ii = 2, nx-1
		do jj = 2, ny-1
			res_ec_dif = (ttt(ii,jj,1)+ttt(ii,jj,3))/(deltax*deltax) 
			res_ec_dif = res_ec_dif + (ttt(ii,jj,4)+ttt(ii,jj,5))/(deltay*deltay) 
			res_ec_dif = res_ec_dif - ttt(ii,jj,2)*((2.d0/(deltax*deltax)) + (2.d0/(deltay*deltay)))
		end do 
	end do Ec_diferencial_parcial
	!$omp end parallel do
	
	
end subroutine res_temp

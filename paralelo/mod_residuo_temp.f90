module utiles
  !
  implicit none
  !
contains
  !
  Subroutine residuo_temp(temp,deltax,deltay,resid)
    !
    ! Esta subrutina eval'ua el laplaciano de un campo escalar y devuelve
    ! su elemento m'as grande
    !
    use omp_lib
    !
    implicit none
    !
    double precision, intent(in)  :: temp(:,:)
    double precision, intent(in)  :: deltax,deltay
    double precision, intent(out) :: resid
    !
    integer  ::  sx, sy
    integer  ::  ii, jj
    !
    ! ----------------------------------------------
    !
    ! Se determinan las dimensiones del arreglo temp
    !
    sx = size(temp,1)
    sy = size(temp,dim=2)
    !
    ! Se recorren los puntos de la malla
    !
    ! do jj = 2, sy-1
    !    !
    !    do ii = 2, sx-1
    !       !
    !       resid = ( temp(ii-1,jj,1)-2.d0*temp(ii,jj,1) + temp(ii+1,jj,1) )/(deltax*deltax)+&
    !            & ( temp(ii,jj-1,1)-2.d0*temp(ii,jj,1) + temp(ii,jj+1,1) )/(deltay*deltay)
    !    end do
    !    !
    ! end do
    !
  end Subroutine residuo_temp
  !
end module utiles

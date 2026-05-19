subroutine tri(a,b,c,r,u,nn)
  !
  ! Subrutina que resuelve el problema usando eliminaci\'on gaussiana
  !
  ! Ax=r donde A es una matriz tridiagonal con
  ! elementos a, b, c
  ! y devuelve el valor de u
  !
  implicit none
  !
  integer,          intent(in)    :: nn
  double precision, intent(in)    :: a(nn),c(nn)
  double precision, intent(inout) :: b(nn),r(nn)
  double precision, intent(inout) :: u(nn)
  integer :: ii
  double precision :: m

  ! Eliminaci\'on elementos bajo la matriz
  gausselim: do ii=2,nn
     m = a(ii)/b(ii-1)
     r(ii)=r(ii)-m*r(ii-1)
     b(ii)=b(ii)-m*c(ii-1)
  end do gausselim
  !
  ! Soluci\'on para u
  !
  u(nn)=r(nn)/b(nn)
  sustatras: do ii=nn-1,1,-1
     u(ii)=(r(ii)-c(ii)*u(ii+1))/b(ii)
  end do sustatras
  !
end subroutine tri

subroutine tri_factor(a,b,c,cp,den,nn)
  !
  ! Factorizacion de Thomas para una matriz tridiagonal constante.
  !
  implicit none
  !
  integer,          intent(in)  :: nn
  double precision, intent(in)  :: a(nn),b(nn),c(nn)
  double precision, intent(out) :: cp(nn),den(nn)
  integer :: ii

  den(1) = b(1)
  cp(1)  = c(1)/den(1) ! cp \equiv c'

  do ii=2,nn-1
     den(ii) = b(ii) - a(ii)*cp(ii-1)
     cp(ii) = c(ii)/den(ii)
  end do
  den(nn) = b(nn) - a(nn)*cp(nn-1)
  cp(nn) = 0.d0
end subroutine tri_factor

subroutine tri_solve(a,cp,den,r,u,nn)
  !
  ! Sustituci\'on usando la factorizacion calculada por tri_factor.
  ! Modifica r in-place para guardar el lado derecho transformado.
  !
  implicit none
  !
  integer,          intent(in)    :: nn
  double precision, intent(in)    :: a(nn),cp(nn),den(nn)
  double precision, intent(inout) :: r(nn)
  double precision, intent(inout) :: u(nn)
  integer :: ii

  r(1) = r(1)/den(1)
  do ii=2,nn
     r(ii) = (r(ii) - a(ii)*r(ii-1))/den(ii)
  end do

  u(nn) = r(nn)
  do ii=nn-1,1,-1
     u(ii) = r(ii) - cp(ii)*u(ii+1)
  end do
end subroutine tri_solve

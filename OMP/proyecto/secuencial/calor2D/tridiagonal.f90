subroutine tri(a,b,c,r,u,n)
  !
  ! Subrutina que resuelve el problema
  !
  ! Ax=r donde A es una matriz tridiagonal con
  ! elementos a, b, c
  ! y devuelve el valor de u
  !
  implicit none
  !
  integer,          intent(in)    :: n
  double precision, intent(in)    :: a(n),c(n)
  double precision, intent(inout) :: b(n),r(n)
  double precision, intent(inout) :: u(n)
  integer :: ii
  double precision :: m

  ! eliminacion elementos bajo la matriz
  gausselim: do ii=2,n
     m = a(ii)/b(ii-1)
     r(ii)=r(ii)-m*r(ii-1)
     b(ii)=b(ii)-m*c(ii-1)
  end do gausselim
  !
  ! solucion para u
  !
  u(n)=r(n)/b(n)
  sustatras: do ii=n-1,1,-1
     u(ii)=(r(ii)-c(ii)*u(ii+1))/b(ii)
  end do sustatras
  !
end subroutine tri

subroutine tri_factor(a,b,c,cp,den,n)
  !
  ! Factorizacion de Thomas para una matriz tridiagonal constante.
  !
  implicit none
  !
  integer,          intent(in)  :: n
  double precision, intent(in)  :: a(n),b(n),c(n)
  double precision, intent(out) :: cp(n),den(n)
  integer :: ii

  den(1) = b(1)
  cp(1)  = c(1)/den(1)

  do ii=2,n-1
     den(ii) = b(ii) - a(ii)*cp(ii-1)
     cp(ii) = c(ii)/den(ii)
  end do
  den(n) = b(n) - a(n)*cp(n-1)
  cp(n) = 0.d0
end subroutine tri_factor

subroutine tri_solve(a,cp,den,r,u,n)
  !
  ! Sustitucion usando la factorizacion calculada por tri_factor.
  ! Modifica r in-place para guardar el lado derecho transformado.
  !
  implicit none
  !
  integer,          intent(in)    :: n
  double precision, intent(in)    :: a(n),cp(n),den(n)
  double precision, intent(inout) :: r(n)
  double precision, intent(inout) :: u(n)
  integer :: ii

  r(1) = r(1)/den(1)
  do ii=2,n
     r(ii) = (r(ii) - a(ii)*r(ii-1))/den(ii)
  end do

  u(n) = r(n)
  do ii=n-1,1,-1
     u(ii) = r(ii) - cp(ii)*u(ii+1)
  end do
end subroutine tri_solve

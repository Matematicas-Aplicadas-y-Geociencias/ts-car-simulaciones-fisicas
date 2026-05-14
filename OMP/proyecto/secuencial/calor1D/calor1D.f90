Program Calor1D
  !
  Implicit none
  !
  ! Declaracion de variables
  !
  ! Iteradores y tamaño del problema
  !
  integer :: ii
  integer, parameter :: L = 10
  integer, parameter :: nn = 60
  double precision, parameter :: q0_sobre_k = -1.d0
  !
  ! Variables del dominio computacional
  !
  double precision :: deltax
  !
  ! Variables del problema fisico
  !
  ! double precision :: aa(nn,nn) ! matriz para el problema
  double precision :: tt(nn)    ! vector de incognitas
  double precision :: rr(nn)    ! vector de resultados del s. ecuaciones
  !
  double precision :: aa(nn), bb(nn), cc(nn) ! Variables para almacenar
  !                                          ! matriz tridiagonal sobredimensionada
  double precision :: cp(nn), den(nn)
  !
  ! Dominio computacional
  !
  deltax  = dble(L)/(nn - 1)
  ! Inicializacion de variables
  !
  aa(:)   = 0.d0
  bb(:)   = 0.d0
  cc(:)   = 0.d0
  rr(:)   = 0.d0
  !
  ! Ensamblar la matriz tridiagonal y el vector de resultados
  !
  do ii = 2, nn-1
     !
     aa(ii) = 1.d0
     bb(ii) =-2.d0
     cc(ii) = 1.d0
     rr(ii) = 0.d0
     !
  end do
  !
  ! Impone cond. frontera
  !
  aa(1)     = 0.d0 ! no se usa en los c'alculos
  bb(1)     =-1.d0
  cc(1)     = 1.d0
  rr(1)     = q0_sobre_k * deltax
  !  
  aa(nn)    = 0.d0
  bb(nn)    = 1.d0
  cc(nn)    = 0.d0 ! no se usa en los c'alculos
  rr(nn)    = 0.d0
  !
  ! Resolver el problema algebraico
  !
  call tri_factor(aa,bb,cc,cp,den,nn)
  call tri_solve(aa,cp,den,rr,tt,nn)
  !
  ! Postproceso
  !
  do ii = 1, nn
     write(101,*) (ii-1)*deltax, tt(ii)
  end do
  !
end Program Calor1D

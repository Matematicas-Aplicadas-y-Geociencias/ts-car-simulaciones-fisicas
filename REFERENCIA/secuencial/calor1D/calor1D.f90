Program Calor1D
  !
  Implicit none
  !
  ! Declaracion de variables
  !
  ! Iteradores y tamaño del problema
  !
  integer :: ii, jj
  integer, parameter :: nn = 60
  !
  ! Variables del dominio computacional
  !
  double precision :: ll, deltax
  !
  ! Variables del problema fisico
  !
  double precision :: xx(nn)    ! variable de la malla
  ! double precision :: aa(nn,nn) ! matriz para el problema
  double precision :: tt(nn)    ! vector de incognitas
  double precision :: rr(nn)    ! vector de resultados del s. ecuaciones
  double precision :: cf(2)     ! vector de condiciones de frontera
  !
  double precision :: aa(nn), bb(nn), cc(nn) ! Variables para almacenar
  !                                          ! matriz tridiagonal sobredimensionada
  !
  ! Dominio computacional
  !
  ll      = 10.d0
  deltax  = 10.d0/nn
  ! Inicializacion de variables
  !
  xx(:)   = 0.d0
  aa(:)   = 0.d0
  bb(:)   = 0.d0
  cc(:)   = 0.d0
  tt(:)   =-1.d0
  rr(:)   = 0.d0
  !
  ! Condiciones de frontera
  !
  cf(1) =-1.d0
  cf(2) = 0.d0
  !
  ! Ensamblar la matriz
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
  rr(1)     = cf(1) * deltax
  !  
  aa(nn)    = 0.d0
  bb(nn)    = 1.d0
  cc(nn)    = 0.d0 ! no se usa en los c'alculos
  rr(nn)    = cf(2)
  !
  ! Resolver el problema algebraico
  !
  call tri(aa,bb,cc,rr,tt,nn)
  !
  ! Postproceso
  !
  write(*,*) aa(:)
  write(*,*) bb(:)
  write(*,*) cc(:)
  write(*,*) tt(:)
  !
  do ii = 1, nn
     write(101,*) (ii-1)*deltax, tt(ii)
  end do
  !
end Program Calor1D

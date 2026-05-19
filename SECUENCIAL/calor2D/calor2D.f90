Program Calor2D
  !
  Implicit none
  !
  ! Declaraci\'on de variables
  !
  ! Iteradores y tamaño del problema
  !
  integer :: ii, jj, iter
  integer, parameter :: nx = 60, ny = 30, itermax=10000
  !
  ! Variables del dominio computacional
  !
  double precision :: lx, ly, deltax, deltay
  double precision :: inv_dx2, inv_dy2
  !
  ! Variable para el residuo de iteraciones, y tolerancia
  !
  double precision :: residuo, tolerancia, tolerancia2
  !
  ! Variables del problema f\'isico
  !
  double precision :: tt(nx,ny,2)     ! vector de inc\'ognitas, 1 para la iteraci\'on actual y 2 para la anterior
  double precision :: tx(nx), ty(ny)  ! vectores de inc\'ognitas 1D
  double precision :: rx(nx),ry(ny)   ! vector de resultados del s. ecuaciones
  double precision :: cfx(ny,2),cfy(nx,2) ! vector de condiciones de frontera
  !
  ! Variables para almacenar los coeficientes de los sistemas tridiagonales
  !
  double precision :: ax(nx), cx(nx), bx_base(nx) ! Variables para almacenar
  !                                               ! matriz tridiagonal sobredimensionada
  double precision :: cpx(nx), denx(nx)
  !
  double precision :: ay(ny), cy(ny), by_base(ny) ! Variables para almacenar
  !                                               ! matriz tridiagonal sobredimensionada
  double precision :: cpy(ny), deny(ny)
  !
  ! Tolerancia
  !
  tolerancia = 1d-3
  tolerancia2 = tolerancia*tolerancia
  !
  ! Dominio computacional
  !
  lx      = 10.d0
  deltax  = lx/(nx - 1) ! nx puntos definen (nx - 1) intervalos
  ly      = 5.d0
  deltay  = ly/(ny - 1) ! ny puntos definen (ny - 1) intervalos
  inv_dx2 = 1.d0/(deltax*deltax)
  inv_dy2 = 1.d0/(deltay*deltay)
  !
  ! Inicializacion de variables
  !
  ax(:)   = 0.d0
  bx_base(:) = 0.d0
  cx(:)   = 0.d0
  rx(:)   = 0.d0
  ay(:)   = 0.d0
  by_base(:) = 0.d0
  cy(:)   = 0.d0
  ry(:)   = 0.d0
  tt(:,:,:) = 0.d0 ! Valores iniciales de temperatura, se pueden modificar
                   ! para probar diferentes condiciones iniciales
  !
  ! Coeficientes constantes de los sistemas tridiagonales.
  !
  do ii = 2, nx-1
     ax(ii) = inv_dx2
     bx_base(ii) = -2.d0*(inv_dx2 + inv_dy2)
     cx(ii) = inv_dx2
  end do
  ax(1) = 0.d0
  bx_base(1) = 1.d0
  cx(1) = 0.d0
  ax(nx) = 0.d0
  bx_base(nx) = 1.d0
  cx(nx) = 0.d0
  !
  do jj = 2, ny-1
     ay(jj) = inv_dy2
     by_base(jj) = -2.d0*(inv_dx2 + inv_dy2)
     cy(jj) = inv_dy2
  end do
  ay(1) = 0.d0
  by_base(1) = -1.d0
  cy(1) = 1.d0
  ay(ny) = -1.d0
  by_base(ny) = 1.d0
  cy(ny) = 0.d0
  !
  call tri_factor(ax,bx_base,cx,cpx,denx,nx)
  call tri_factor(ay,by_base,cy,cpy,deny,ny)
  !
  ! Condiciones de frontera en direcci'on x
  !
  do ii = 1, ny
     cfx(ii,1) = 1.d0
     cfx(ii,2) = 0.d0
  end do
  !
  ! Condiciones de frontera en direcci'on y
  !
  do jj = 1, nx
     cfy(jj,1) = 0.d0
     cfy(jj,2) = 0.d0
  end do
  !
  bucle_iteraciones: do iter = 1, itermax
     !
     ! Inicializamos el valor de la iteraci'on anterior
     !
     tt(:,:,2) = tt(:,:,1)
     !
     barrido_y: do jj = 2, ny-1
        ensambla_tri_x: do ii = 2, nx-1

           rx(ii) = -inv_dy2*tt(ii,jj-1,1) - inv_dy2*tt(ii,jj+1,1)
           
        end do ensambla_tri_x
        !
        ! Impone cond. frontera
        !
        rx(1)     = cfx(jj,1)
        !
        rx(nx)    = cfx(jj,2)
        !
        ! Resolver el problema algebraico
        !
        call tri_solve(ax,cpx,denx,rx,tx,nx)
        !
        ! Actualizar la temperatura de la placa
        !
        do ii = 1, nx
           
           tt(ii,jj,1) = tx(ii)
           
        end do
        !
     end do barrido_y

     barrido_x: do ii = 2, nx-1
        ensambla_tri_y: do jj = 2, ny-1

           ry(jj) = -inv_dx2*tt(ii-1,jj,1) - inv_dx2*tt(ii+1,jj,1)
           
        end do ensambla_tri_y
        !
        ! Impone cond. frontera
        !
        ry(1)     = cfy(ii,1)
        !
        ry(ny)    = cfy(ii,2)
        !
        ! Resolver el problema algebraico
        !
        call tri_solve(ay,cpy,deny,ry,ty,ny)
        !
        !
        ! Actualizar la temperatura de la placa
        !
        do jj = 1, ny
           
           tt(ii,jj,1) = ty(jj)
           
        end do
        !
     end do barrido_x
     !
     ! Criterio de convergencia
     !
     residuo = 0.d0
     do jj = 1, ny
        do ii = 1, nx
           residuo = residuo + (tt(ii,jj,1)-tt(ii,jj,2))*(tt(ii,jj,1)-tt(ii,jj,2))
        end do
     end do
     !
     ! write(*,*) "DEBUG: ", iter, residuo
     !
     if( residuo < tolerancia2 )exit
     !
  end do bucle_iteraciones
  write(*,*) "Convergencia en ", iter, " iteraciones"
  !
  do jj = 1, ny
     do ii = 1, nx
        write(*,*) (ii-1)*deltax, (jj-1)*deltay, tt(ii,jj,1)
     end do
     write(*,*) ' '
  end do
  !
end Program Calor2D

Program Calor2D
  !
  use omp_lib
  !
  use utiles, only : postproceso_vtk
  use utiles, only : residuo_temp
  use utiles, only : nx, ny, itermax
  !
  Implicit none
  !
  ! Declaracion de variables
  !
  ! Iteradores y tamaño del problema
  !
  integer :: ii, jj, iter
  !
  ! Variables del dominio computacional
  !
  double precision :: lx, ly, deltax, deltay
  !
  ! Variable para el residuo de iteraciones, y tolerancia
  !
  double precision :: residuo, tolerancia, resid_u
  !
  ! Variables del problema fisico
  !
  double precision :: xx(nx), yy(ny)  ! variables de la malla
  double precision :: tt(nx,ny,2)     ! vector de incognitas, 1 para la iteraci'on actual y 2 para la anterior
  double precision :: resid_tt(nx,ny) ! vector de residuo
  double precision :: tx(nx), ty(ny)  ! vectores de incognitas 1D
  double precision :: rx(nx),ry(ny)   ! vector de resultados del s. ecuaciones
  double precision :: cfx(ny,2),cfy(nx,2) ! vector de condiciones de frontera
  !
  double precision :: ax(nx), bx(nx), cx(nx) ! Variables para almacenar
  !                                          ! matriz tridiagonal sobredimensionada
  !
  double precision :: ay(ny), by(ny), cy(ny) ! Variables para almacenar
  !                                          ! matriz tridiagonal sobredimensionada
  !
  ! Variables de postproceso
  !
  character(48) :: archivo
  !
  ! Tolerancia
  !
  tolerancia = 1d-4
  !
  ! Dominio computacional
  !
  lx      = 10.d0
  deltax  = lx/nx
  ly      = 5.d0
  deltay  = ly/ny
  do jj = 1, ny
     yy(jj)=(jj-1)*deltay
  end do
  do ii = 1, nx
     xx(ii)=(ii-1)*deltax
  end do   
  !
  ! Inicializacion de variables
  !
  ! xx(:)   = 0.d0
  ax(:)   = 0.d0
  bx(:)   = 0.d0
  cx(:)   = 0.d0
  rx(:)   = 0.d0
  ay(:)   = 0.d0
  by(:)   = 0.d0
  cy(:)   = 0.d0
  ry(:)   = 0.d0
  tt(:,:,:) = 0.d0
  !
  ! Condiciones de frontera en direcci'on x
  !
  ! Estos bucles pueden paralelizarse, pero hay que valorar bien la cantidad
  ! de trabajo que se va a compartir respecto al tiempo necesario para abrir
  ! los hilos paralelos.
  !
  do ii = 1, ny
     cfx(ii,1) = 1.d0
     cfx(ii,2) = 0.d0
  end do
  !
  ! Condiciones de frontera en direcci'on y
  !
  do jj = 1, nx
     cfy(jj,1) = 1.d0
     cfy(jj,2) = 0.d0
  end do
  !
  bucle_iteraciones: do iter = 1, itermax
     !
     ! Inicializamos el valor de la iteraci'on anterior
     !
     tt(:,:,2) = tt(:,:,1)
     !
     !---------------------------------------------------------------
     !
     ! Paralelizamos el barrido en la direcci'on y en bandas
     ! compuestas por grupos de l'ineas, observamos que si usamos pocas
     ! l'ineas tenemos una aceleraci'on pobre o ausente
     !
     !$omp parallel do default(none) &
     !$omp shared(  deltax, deltay, tt, cfx) &
     !$omp private( ax, bx, cx, rx, tx, ii )
     barrido_y: do jj = 2, ny-1
        !
        ! Es posible combinar directivas de openmp, por ejemplo,
        ! si necesitamos una regi'on paralela unicamente para un bucle,
        ! podemos combinar "parallel" con "do"
        !
        ensambla_tri_x: do ii = 2, nx-1

           ax(ii) = 1.d0/(deltax*deltax)
           bx(ii) =-2.d0*(1.d0/(deltax*deltax)+ 1.d0/(deltay*deltay))
           cx(ii) = 1.d0/(deltax*deltax)
           rx(ii) =-1.d0/(deltay*deltay)*tt(ii,jj-1,1)-1.d0/(deltay*deltay)&
                & * tt(ii,jj+1,1)
           
        end do ensambla_tri_x
        !
        ! Impone cond. frontera
        !
        ax(1)     = 0.d0 ! no se usa en los c'alculos
        bx(1)     = 1.d0
        cx(1)     = 0.d0
        rx(1)     = cfx(jj,1)
        !
        ax(nx)    = 0.d0 
        bx(nx)    = 1.d0
        cx(nx)    = 0.d0 ! no se usa en los c'alculos
        rx(nx)    = cfx(jj,2)
        !
        ! Resolver el problema algebraico
        !
        call tri(ax,bx,cx,rx,tx,nx)
        !
        ! Actualizar la temperatura de la placa
        !
        do ii = 1, nx
           
           tt(ii,jj,1) = tx(ii)
           
        end do
        !
     end do barrido_y
     !$omp end parallel do
     !
     !---------------------------------------------------------------
     !
     ! Paralelizamos el barrido en la direcci'on x en bandas
     ! compuestas por grupos de l'ineas, observamos que si usamos pocas
     ! l'ineas tenemos una aceleraci'on pobre o ausente
     !
     !$omp parallel do default(none) &
     !$omp shared(  deltax, deltay, tt, cfy) &
     !$omp private( ay, by, cy, ry, ty )
     barrido_x: do ii = 2, nx-1

        ensambla_tri_y: do jj = 2, ny-1

           ay(jj) = 1.d0/(deltay*deltay)
           by(jj) =-2.d0*(1.d0/(deltax*deltax)+ 1.d0/(deltay*deltay))
           cy(jj) = 1.d0/(deltay*deltay)
           ry(jj) =-1.d0/(deltax*deltax)*tt(ii-1,jj,1)-1.d0/(deltax*deltax)*&
                tt(ii+1,jj,1)
           
        end do ensambla_tri_y
        !
        ! Impone cond. frontera
        !
        ay(1)     = 0.d0 ! no se usa en los c'alculos
        by(1)     = 1.d0
        cy(1)     = 0.d0
        ry(1)     = cfy(ii,1)
        !
        ay(ny)    =-1.d0 
        by(ny)    = 1.d0
        cy(ny)    = 0.d0 ! no se usa en los c'alculos
        ry(ny)    = cfy(ii,2)
        !
        ! Resolver el problema algebraico
        !
        call tri(ay,by,cy,ry,ty,ny)
        !
        !
        ! Actualizar la temperatura de la placa
        !
        do jj = 1, ny
           
           tt(ii,jj,1) = ty(jj)
           
        end do
        !
     end do barrido_x
     !$omp end parallel do
     !
     ! Criterio de convergencia
     !
     residuo = 0.d0
     do ii = 1, nx
        do jj = 1, ny
           residuo = residuo + (tt(ii,jj,1)-tt(ii,jj,2))*(tt(ii,jj,1)-tt(ii,jj,2))
        end do
     end do
     !
     residuo = sqrt(residuo)
     !
     ! write(*,*) "DEBUG: ", iter, residuo
     !
     if( residuo < tolerancia )exit
     !
  end do bucle_iteraciones
  !
  write(*,*) "Convergencia en ", iter, " iteraciones"
  !
  call residuo_temp( tt(1:nx,1:ny,1), deltax, deltay, resid_tt )
  !
  ! Aunque es muy tentador, no podemos paralelizar este bucle,
  ! el archivo queda desordenado y gnuplot (y otros graficadores) no
  ! los procesan bien.
  !
  archivo = 'salida.vtk'
  !
  call postproceso_vtk(xx,yy,tt(1:nx,1:ny,1), resid_tt ,archivo)
  !
end Program Calor2D

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
  integer :: ii, jj, iter, k
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
  double precision :: tt(nx*ny,2)     ! vector de incognitas, 1 para la iteraci'on actual y 2 para la anterior
  double precision :: resid_tt(nx,ny) ! vector de residuo
  double precision :: tx(nx),ty(ny) ! vectores de incognitas 1D
  double precision :: rr(nx*ny)   ! vector de resultados del s. ecuaciones
  double precision :: cfx(nx,2), cfy(ny,2)! vector de condiciones de frontera
  integer :: ind_in, ind_f  !indices de segmentos de la matriz
  !
  double precision :: aa(nx*ny), bb(nx*ny), cc(nx*ny) ! Variables para almacenar
  !                                          ! matriz tridiagonal sobredimensionada
  !

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
  aa(:)   = 0.d0
  bb(:)   = 0.d0
  cc(:)   = 0.d0
  rr(:)   = 0.d0
  tt(:,:) = 0.d0
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
     !$omp parallel do
     do k = 2, (ny-1)*(nx-1)
           tt(k,2) = tt(k,1)
     end do
     !$omp end parallel do
     !
     !---------------------------------------------------------------
     !
     ! Paralelizamos el barrido en la direcci'on y en bandas
     ! compuestas por grupos de l'ineas, observamos que si usamos pocas
     ! l'ineas tenemos una aceleraci'on pobre o ausente
     !
     !$omp parallel do default(none) &
     !$omp shared(  deltax, deltay, tt, cfx) &
     !$omp private( aa, bb, cc, rr, tx,ind_in,ind_f)
     barrido_y: do jj = 2, ny-1
        !
        ! Es posible combinar directivas de openmp, por ejemplo,
        ! si necesitamos una regi'on paralela unicamente para un bucle,
        ! podemos combinar "parallel" con "do"
        !
        ind_in = (jj-1)*nx + 1
        ind_f  = jj*nx

        ensambla_tri_x: do ii = 2, nx-1

           aa((jj-1)*nx+ii) = 1.d0/(deltax*deltax)
           bb((jj-1)*nx+ii) =-2.d0*(1.d0/(deltax*deltax)+ 1.d0/(deltay*deltay))
           cc((jj-1)*nx+ii) = 1.d0/(deltax*deltax)
           rr((jj-1)*nx+ii) =-1.d0/(deltay*deltay)*tt((jj-2)*nx+ii,1)-1.d0/(deltay*deltay)&
                & * tt((jj)*nx+ii,1)
           
        end do ensambla_tri_x
        !
        ! Impone cond. frontera
        !
        aa((jj-1)*nx+1)     = 0.d0 ! no se usa en los c'alculos
        bb((jj-1)*nx+1)     = 1.d0
        cc((jj-1)*nx+1)     = 0.d0
        rr((jj-1)*nx+1)     = cfx(jj,1)
        !
        aa((jj)*nx)    = 0.d0
        bb((jj)*nx)    = 1.d0
        cc((jj)*nx)    = 0.d0 ! no se usa en los c'alculos
        rr((jj)*nx)    = cfx(jj,2)
        !
        ! Resolver el problema algebraico
        !
        call tri(aa(ind_in:ind_f),bb(ind_in:ind_f),cc(ind_in:ind_f),rr(ind_in:ind_f),tx,nx)
        !
        ! Actualizar la temperatura de la placa
        !
        do ii = 1, nx
           
           tt((ii-1)*ny+jj,1) = tx(ii)
           
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
     ind_in = 0
     ind_f  = 0
     !$omp parallel do default(none) &
     !$omp shared(  deltax, deltay, tt, cfy) &
     !$omp private( aa, bb, cc, rr, ty,ind_in,ind_f)
     barrido_x: do ii = 2, nx-1
       ind_in = (ii-1)*ny + 1
       ind_f  = ii*ny


        ensambla_tri_y: do jj = 2, ny-1

           aa((ii-1)*ny+jj)  = 1.d0/(deltay*deltay)
           bb((ii-1)*ny+jj) =-2.d0*(1.d0/(deltax*deltax)+ 1.d0/(deltay*deltay))
           cc((ii-1)*ny+jj) = 1.d0/(deltay*deltay)
           rr((ii-1)*ny+jj) =-1.d0/(deltax*deltax)*tt((ii-2)*ny+jj,1)-1.d0/(deltax*deltax)*&
                tt((ii)*ny+jj,1)
           
        end do ensambla_tri_y
        !
        ! Impone cond. frontera
        !
        aa((ii-1)*ny+1)     = 0.d0 ! no se usa en los c'alculos
        bb((ii-1)*ny+1)     = 1.d0
        cc((ii-1)*ny+1)     = 0.d0
        rr((ii-1)*ny+1)     = cfy(ii,1)
        !
        aa(ii*ny)    =-1.d0
        bb(ii*ny)    = 1.d0
        cc(ii*ny)    = 0.d0 ! no se usa en los c'alculos
        rr(ii*ny)    = cfy(ii,2)
        !
        ! Resolver el problema algebraico
        !
        call tri(aa(ind_in:ind_f),bb(ind_in:ind_f),cc(ind_in:ind_f),rr(ind_in:ind_f),ty,ny)
        !
        !
        ! Actualizar la temperatura de la placa
        !
        do jj = 1, ny
           
           tt((ii-1)*ny+jj,1) = ty(jj)
           
        end do
        !
     end do barrido_x
     !$omp end parallel do
     !
     ! Criterio de convergencia
     !
     residuo = 0.d0
     !$omp parallel do reduction(+:residuo)
     do ii = 2, nx-1
        do jj = 2, ny-1
           
           residuo = residuo + (tt((jj-1)*nx+ii,1)-tt((jj-1)*nx+ii,2))*(tt((jj-1)*nx+ii,1)-tt((jj-1)*nx+ii,2))
           
        end do
     end do
     !$omp end parallel do
     !
     residuo = sqrt(residuo)
     !
     ! write(*,*) "DEBUG: ", iter, residuo
     !
     ! if( residuo < tolerancia )exit
     !
  end do bucle_iteraciones
  !
  write(*,*) "Convergencia en ", iter, " iteraciones"
  !
  ! call residuo_temp( tt(1:nx,1:ny,1), deltax, deltay, resid_tt )
  !
  ! Aunque es muy tentador, no podemos paralelizar este bucle,
  ! el archivo queda desordenado y gnuplot (y otros graficadores) no
  ! los procesan bien.
  !
  !archivo = 'salida.vtk'
  write(*,*),tt
  ! call postproceso_vtk(xx,yy,tt(1:nx,1:ny,1), resid_tt ,archivo)
  !
end Program Calor2D

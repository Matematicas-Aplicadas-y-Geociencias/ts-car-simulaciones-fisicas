Program Calor2D
  !
  use omp_lib
  !
  use utiles, only : postproceso_vtk
  use utiles, only : residuo_temp
  use utiles, only : nx, ny, itermax
  use utiles, only : indicex, indicey
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
  double precision :: tt(nx*ny), tr(nx*ny), tt_o(nx*ny)    ! vector de incognitas, 1 para la iteraci'on actual y 2 para la anterior
  double precision :: tt_m(nx,ny) !forma matricial de tt
  double precision :: resid_tt(nx,ny) ! vector de residuo
  double precision :: rr(nx*ny)   ! vector de resultados del s. ecuaciones
  double precision :: cfx(ny,2), cfy(nx,2)! vector de condiciones de frontera
  integer :: ind_in, ind_f  !indices de segmentos de la matriz
  !
  double precision :: aa(nx*ny), bb(nx*ny), cc(nx*ny) ! Variables para almacenar
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
  !$acc parallel loop
  do jj = 1, ny
     yy(jj)=(jj-1)*deltay
  end do
  !$acc end parallel loop
  !
  !$acc parallel loop
  do ii = 1, nx
     xx(ii)=(ii-1)*deltax
  end do   
  !$acc end parallel loop
  !
  ! Inicializacion de variables
  !
  ! xx(:)   = 0.d0
  aa(:)   = 0.d0
  bb(:)   = 0.d0
  cc(:)   = 0.d0
  rr(:)   = 0.d0
  tt(:) = 0.d0
  tr(:) = 0.d0
  !
  ! Condiciones de frontera en direcci'on x
  !
  ! Estos bucles pueden paralelizarse, pero hay que valorar bien la cantidad
  ! de trabajo que se va a compartir respecto al tiempo necesario para abrir
  ! los hilos paralelos.
  !
  !$acc parallel loop
  do ii = 1, ny
     cfx(ii,1) = 1.d0
     cfx(ii,2) = 0.d0
  end do
  !$acc end parallel loop
  !
  ! Condiciones de frontera en direcci'on y
  !
  !$acc parallel loop
  do jj = 1, nx
     cfy(jj,1) = 1.d0
     cfy(jj,2) = 0.d0
  end do
  !$acc end parallel loop
  !
  bucle_iteraciones: do iter = 1, itermax
     !
     tt_o(:)=tt(:)
     !---------------------------------------------------------------
     !
     ! Paralelizamos el barrido en la direcci'on y en bandas
     ! compuestas por grupos de l'ineas, observamos que si usamos pocas
     ! l'ineas tenemos una aceleraci'on pobre o ausente
     !
     !$acc parallel loop 
     !default(none) &
     !!$acc shared( deltax, deltay, tt, tr, cfx, &
     !!$acc aa, bb, cc, rr)
     barrido_y: do jj = 2, ny-1
        !
        ! Es posible combinar directivas de openmp, por ejemplo,
        ! si necesitamos una regi'on paralela unicamente para un bucle,
        ! podemos combinar "parallel" con "do"

        ensambla_tri_x: do ii = 2, nx-1

           aa((jj-1)*nx+ii) = 1.d0/(deltax*deltax)
           bb((jj-1)*nx+ii) =-2.d0*(1.d0/(deltax*deltax)+ 1.d0/(deltay*deltay))
           cc((jj-1)*nx+ii) = 1.d0/(deltax*deltax)
           rr((jj-1)*nx+ii) =-1.d0/(deltay*deltay)*tt(indicex(ii,jj-1))-1.d0/(deltay*deltay)&
                & * tt(indicex(ii,jj+1))
           
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
    end do barrido_y
    !$acc end parallel loop


    !$acc parallel loop routine seq
    !default(none) &
    !!$acc shared( tt, aa, bb, cc, rr )
    inversor_y: do jj = 2, ny-1
    call tri(aa(indicex(1,jj):indicex(nx,jj)), &
         bb(indicex(1,jj):indicex(nx,jj)), &
         cc(indicex(1,jj):indicex(nx,jj)), &
         rr(indicex(1,jj):indicex(nx,jj)), &
         tt(indicex(1,jj):indicex(nx,jj)), &
         nx)
	
        !
     end do inversor_y
     !$acc end parallel loop
     !
     !TRASNPONER
     !
     !
     !
     !$acc parallel loop collapse(2) 
     !default(none) &
     !!$acc shared(tt,tr) &
     !!$acc private(ii,jj)
      do jj=1,ny
        do ii=1,nx

          tr(indicey(ii,jj)) = &
              tt(indicex(ii,jj))

        end do
     end do
     !$acc end parallel loop
     !---------------------------------------------------------------
     !
     ! Paralelizamos el barrido en la direcci'on x en bandas
     ! compuestas por grupos de l'ineas, observamos que si usamos pocas
     ! l'ineas tenemos una aceleraci'on pobre o ausente

     !$acc parallel loop
     !default(none) &
     !!$omp shared(deltax, deltay, tr, cfy, &
     !!$omp aa, bb, cc, rr)
     barrido_x: do ii = 2, nx-1

        ensambla_tri_y: do jj = 2, ny-1

           aa((ii-1)*ny+jj)  = 1.d0/(deltay*deltay)
           bb((ii-1)*ny+jj) =-2.d0*(1.d0/(deltax*deltax)+ 1.d0/(deltay*deltay))
           cc((ii-1)*ny+jj) = 1.d0/(deltay*deltay)
           rr((ii-1)*ny+jj) =-1.d0/(deltax*deltax)*tr(indicey(ii-1,jj))-1.d0/(deltax*deltax)*&
                tr(indicey(ii+1,jj))
           
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
     end do barrido_x
     !$acc end parallel loop
        
     ! Resolver el problema algebraico
     !$acc parallel loop routine seq
     !default(none) &
     !!$omp shared( tt, aa, bb, cc, rr, tr)
     inversor_x: do ii = 2, nx-1

         call tri(aa(indicey(ii,1):indicey(ii,ny)), &
         bb(indicey(ii,1):indicey(ii,ny)), &
         cc(indicey(ii,1):indicey(ii,ny)), &
         rr(indicey(ii,1):indicey(ii,ny)), &
         tr(indicey(ii,1):indicey(ii,ny)), &  ! <-- Cambia tr por tt
         ny)
        !
        !
        ! Actualizar la temperatura de la placa
        !

     end do inversor_x
     !$acc end parallel loop
     !TRASNPONER
     !$acc parallel loop collapse(2) 
     !default(none) &
     !!$omp shared(tt,tr) &
     !!$omp private(ii,jj)
     do jj=1,ny
       do ii=1,nx

          tt(indicex(ii,jj)) = &
                tr(indicey(ii,jj))

       end do
     end do
     !$acc end parallel loop
     ! Criterio de convergencia
     !
     residuo = 0.d0
     !$acc parallel loop 
     !reduction(+:residuo)
     do ii = 2, nx-1
        do jj = 2, ny-1
           
           residuo = residuo + (tt((jj-1)*nx+ii)-tt_o((jj-1)*nx+ii))*(tt((jj-1)*nx+ii)-tt_o((jj-1)*nx+ii))
           
        end do
     end do
     !$acc end parallel loop
     !
     residuo = sqrt(residuo)
     !
     ! write(*,*) "DEBUG: ", iter, residuo
     !
     ! if( residuo < tolerancia )exit
     !
  end do bucle_iteraciones
  !
  !write(*,*) "Convergencia en ", iter, " iteraciones"
  !$acc parallel loop 
  do jj=1,ny
   	do ii=1,nx
      	tt_m(ii,jj)=tt((jj-1)*nx+ii)
   	end do
  end do
  !$acc end parallel loop 
  !tt_m(:,:) = RESHAPE(tt(:), SHAPE=(/nx, ny/))
  !call residuo_temp( tt(1:nx,1:ny)_m, deltax, deltay, resid_tt )
  !
  ! Aunque es muy tentador, no podemos paralelizar este bucle,
  ! el archivo queda desordenado y gnuplot (y otros graficadores) no
  ! los procesan bien.
  !
  archivo = 'salida.vtk'
  write(*,*) 'tt_m = ', tt_m
  call postproceso_vtk(xx,yy,tt_m(1:nx,1:ny), resid_tt ,archivo)
  !
end Program Calor2D

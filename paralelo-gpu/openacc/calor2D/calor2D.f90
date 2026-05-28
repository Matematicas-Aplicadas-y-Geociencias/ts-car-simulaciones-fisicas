Program Calor2D
  !
  use openacc
  !
  use utiles, only : postproceso_vtk
  use utiles, only : residuo_temp
  use utiles, only : nx, ny, itermax
  use utiles, only : indicex, indicey
  use utiles, only : tri
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
  double precision :: resid_tt(nx,ny)     ! vector de residuo
  double precision :: tx(nx), ty(nx*ny)   ! vectores de incognitas 1D
  double precision :: rx(nx),ry(ny)       ! vector de resultados del s. ecuaciones
  double precision :: cfx(ny,2),cfy(nx,2) ! vector de condiciones de frontera
  !
  double precision :: aa(nx*ny), bb(nx*ny), cc(nx*ny) ! Matrices con forma 1D para guardar todas
                                                      ! las lineas a invertir
  double precision :: rr(nx*ny)
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
  aa(:)   = 0.d0
  bb(:)   = 0.d0
  cc(:)   = 0.d0
  rr(:)   = 0.d0
  !
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
  ! acc data permite controlar los intercambios de informaci'on entre la tarjeta de video
  ! y la cpu. Tiene las siguiente opciones
  !
  ! copy (lista_variables) reserva memoria en la gpu y copia la informacion de la cpu a
  !                        la gpu en la entrada de la region paralela y copia los datos
  !                        de la gpu a la cpu al salir de la region paralela
  !
  ! copyin (lista_variables) reserva memoria en la gpu y copia la informacion de la cpu
  !                          a la gpu solo al inicio de la region paralela
  !
  ! copyout (lista_variables)  reserva memoria en la gpu y copia la informacion de la gpu
  !                            a la cpu solo al final de la region paralela
  !$acc data copy(&
  !$acc           tt(1:nx,1:ny,1:2) &
  !$acc           )&
  !$acc      create(&
  !$acc           aa(1:nx*ny), bb(1:nx*ny), cc(1:nx*ny), rr(1:nx*ny), ty(1:nx*ny) &
  !$acc           )&
  !$acc      copyin(&
  !$acc           deltax, deltay, cfx(1:ny,1:2), cfy(1:nx,1:2) &
  !$acc           )
  bucle_iteraciones: do iter = 1, itermax
     !
     ! Inicializamos el valor de la iteraci'on anterior
     !
     ! Es posible 'colapsar' bucles anidados con la instrucci'on collapse(n).
     ! El compilador combina los 'indices y crea un solo bucle mas grande
     !
     !$acc parallel loop collapse(2)
     do jj = 2, ny-1
        do ii = 2, nx-1
           tt(ii,jj,2) = tt(ii,jj,1)
        end do
     end do
     !$acc end parallel loop
     !
     !---------------------------------------------------------------
     !
     ! Paralelizamos el barrido en la direcci'on y en bandas
     ! compuestas por grupos de l'ineas, observamos que si usamos pocas
     ! l'ineas tenemos una aceleraci'on pobre o ausente
     !
     !$acc parallel loop collapse(2)
     barrido_y: do jj = 2, ny-1
        !
        ! Es posible combinar directivas de openmp, por ejemplo,
        ! si necesitamos una regi'on paralela unicamente para un bucle,
        ! podemos combinar "parallel" con "do"
        !
        ensambla_tri_x: do ii = 2, nx-1
           !
           aa(indicex(ii,jj)) = 1.d0/(deltax*deltax)
           !
           bb(indicex(ii,jj)) =-2.d0*(1.d0/(deltax*deltax)+ 1.d0/(deltay*deltay))
           !
           cc(indicex(ii,jj)) = 1.d0/(deltax*deltax)
           !
           rr(indicex(ii,jj)) =-1.d0/(deltay*deltay)*tt(ii,jj-1,1)-1.d0/(deltay*deltay)&
                & * tt(ii,jj+1,1)
           !
        end do ensambla_tri_x
        !
     end do barrido_y
     !$acc end parallel loop
     !
     ! Impone cond. frontera
     !
     !$acc parallel loop
     impone_cond_frontx: do jj = 2, ny-1
        !
        aa(indicex(1,jj))     = 0.d0 ! no se usa en los c'alculos
        bb(indicex(1,jj))     = 1.d0
        cc(indicex(1,jj))     = 0.d0
        rr(indicex(1,jj))     = cfx(jj,1)
        !
        aa(indicex(nx,jj))     = 0.d0 ! no se usa en los c'alculos
        bb(indicex(nx,jj))     = 1.d0
        cc(indicex(nx,jj))     = 0.d0
        rr(indicex(nx,jj))     = cfx(jj,2)
        !
     end do impone_cond_frontx
     !$acc end parallel loop
     !
     ! Resolvemos los problemas de matrices tridiagonales a la vez
     !
     !$acc parallel loop gang
     inversor_y: do jj = 2, ny-1
        !
        ! Resolver el problema algebraico
        !
        call tri(aa(indicex(1,jj):indicex(nx,jj)),bb(indicex(1,jj):indicex(nx,jj)),&
             cc(indicex(1,jj):indicex(nx,jj)),rr(indicex(1,jj):indicex(nx,jj)),&
             tt(1:nx,jj,1),nx)
        !
        ! Actualizar la temperatura de la placa
        !
        ! do ii = 1, nx
           
        !    tt(ii,jj,1) = tx(ii)
           
        ! end do
        !
     end do inversor_y
     !$acc end parallel loop
     ! $ acc end data
     !
     !---------------------------------------------------------------
     !
     ! Paralelizamos el barrido en la direcci'on x en bandas
     ! compuestas por grupos de l'ineas, observamos que si usamos pocas
     ! l'ineas tenemos una aceleraci'on pobre o ausente
     !
     !$acc parallel loop collapse(2)
     barrido_x: do ii = 2, nx-1

        ensambla_tri_y: do jj = 2, ny-1
           !
           aa(indicey(ii,jj)) = 1.d0/(deltay*deltay)
           !
           bb(indicey(ii,jj)) =-2.d0*(1.d0/(deltax*deltax)+ 1.d0/(deltay*deltay))
           !
           cc(indicey(ii,jj)) = 1.d0/(deltay*deltay)
           !
           rr(indicey(ii,jj)) =-1.d0/(deltax*deltax)*tt(ii-1,jj,1)-1.d0/(deltax*deltax)*&
                tt(ii+1,jj,1)
           !
        end do ensambla_tri_y
        !
     end do barrido_x
     !$acc end parallel loop
     !
     !$acc parallel loop
     cond_front_y: do ii = 2, nx-1
        !
        ! Impone cond. frontera
        !
        aa(indicey(ii,1))     = 0.d0 ! no se usa en los c'alculos
        bb(indicey(ii,1))     = 1.d0
        cc(indicey(ii,1))     = 0.d0
        rr(indicey(ii,1))     = cfy(ii,1)
        !
        aa(indicey(ii,ny))    =-1.d0 
        bb(indicey(ii,ny))    = 1.d0
        cc(indicey(ii,ny))    = 0.d0 ! no se usa en los c'alculos
        rr(indicey(ii,ny))    = cfy(ii,2)
        !
     end do cond_front_y
     !$acc end parallel loop
     !
     ! Resolvemos los problemas de matrices tridiagonales a la vez
     !
     !$acc parallel loop
     inversor_x: do ii = 2, nx-1
        !
        ! Resolver el problema algebraico
        !
        call tri(aa(indicey(ii,1):indicey(ii,ny)),bb(indicey(ii,1):indicey(ii,ny)),&
             cc(indicey(ii,1):indicey(ii,ny)),rr(indicey(ii,1):indicey(ii,ny)),&
             ty(indicey(ii,1):indicey(ii,ny)),ny)
        !
        ! Actualizar la temperatura de la placa
        !
        do jj = 1, ny
           
           tt(ii,jj,1) = ty(indicey(ii,jj))
           
        end do
        !
     end do inversor_x
     !$acc end parallel loop
     !
     ! Criterio de convergencia
     !
     residuo = 0.d0
     !
     !$acc parallel loop reduction(+:residuo)
     do ii = 2, nx-1
        do jj = 2, ny-1
           
           residuo = residuo + (tt(ii,jj,1)-tt(ii,jj,2))*(tt(ii,jj,1)-tt(ii,jj,2))
           
        end do
     end do
     !$acc end parallel
     !
     residuo = sqrt(residuo)
     !
     ! write(*,*) "DEBUG: ", iter, residuo
     !
     ! if( residuo < tolerancia )exit
     !
  end do bucle_iteraciones
  !
  !$acc end data
  !
  write(*,*) "Convergencia en ", iter, " iteraciones"
  !
  ! call residuo_temp( tt(1:nx,1:ny,1), deltax, deltay, resid_tt )
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

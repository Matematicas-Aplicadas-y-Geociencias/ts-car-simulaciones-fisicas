Program Calor2D
  !
  use omp_lib

  Implicit none
  !
  ! Declaracion de variables
  !
  ! Iteradores y tamaño del problema
  !
  integer :: ii, jj, iter, id_thread
  integer, parameter :: nx = 60, ny = 30, itermax=10000
  !
  ! Variables del dominio computacional
  !
  double precision :: lx, ly, deltax, deltay
  !
  ! Variable para el residuo de iteraciones, y tolerancia
  !
  double precision :: residuo, tolerancia
  !
  ! Variables del problema fisico
  !
  double precision :: xx(nx), yy(ny)  ! variables de la malla
  double precision :: tt(nx,ny,2)     ! vector de incognitas, 1 para la iteraci'on actual y 2 para la anterior
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

  double precision :: frac1,frac2
  ! Tolerancia
  !
  tolerancia = 1d-3
  !
  ! Dominio computacional
  !
  lx      = 10.d0
  deltax  = lx/nx
  ly      = 5.d0
  deltay  = ly/ny
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
  !AQUI PARALELIZABLE
  !$omp parallel do private(jj,id_thread) shared(cfx)
  do ii = 1, ny
     cfx(ii,1) = 1.d0
     cfx(ii,2) = 0.d0
  end do
 !$omp end parallel do

  !
  ! Condiciones de frontera en direcci'on y
  !
  !AQUI PARALELIZABLE
  !$omp parallel do private(jj,id_thread) shared(cfy)
  do jj = 1, nx
     id_thread = omp_get_thread_num()
!      write(*,*) "el contador ", jj, " desde el hilo, ", id_thread
     cfy(jj,1) = 0.d0
     cfy(jj,2) = 0.d0
  end do
 !$omp end parallel do

  !
  bucle_iteraciones: do iter = 1, itermax
     !
     ! Inicializamos el valor de la iteraci'on anterior
     !
     tt(:,:,2) = tt(:,:,1)
     !
     barrido_y: do jj = 2, ny-1
     frac1=1.d0*(deltax*deltax) !este y
     frac2=1.d0*(deltay*deltay) !este se repiten varias veces, mejor calcularlo solo una vez.
     !$omp parallel do private(ii) shared(frac1,frac2)

        ensambla_tri_x: do ii = 2, nx-1
           ax(ii) = frac1 
           bx(ii) =-2.d0*(frac1+frac2)
           cx(ii) = frac1
           rx(ii) =-frac2*tt(ii,jj-1,1)-frac2*tt(ii,jj+1,1)
        end do ensambla_tri_x
        !$omp end parallel do
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
  !AQUI PARALELIZABLE
  !$omp parallel do private(ii,id_thread) shared(tt,tx,jj)
  do ii = 1, nx
     id_thread = omp_get_thread_num()
!      write(*,*) "el contador ", jj, " desde el hilo, ", id_thread
           tt(ii,jj,1) = tx(ii)
        end do
 !$omp end parallel do
        !
     end do barrido_y

     barrido_x: do ii = 2, nx-1
        frac1=1.d0/(deltay*deltay)
        frac2=1.d0/(deltax*deltax)
        
        !$omp parallel do private(jj) shared(frac1,frac2)
        ensambla_tri_y: do jj = 2, ny-1
           ay(jj) = frac1
           by(jj) =-2.d0*(frac1+frac2)
           cy(jj) = frac1
           ry(jj) =-frac1*tt(ii-1,jj,1)-frac2*tt(ii+1,jj,1)
        end do ensambla_tri_y
        !$omp end parallel do
        !
        ! Impone cond. frontera
        !
        ay(1)     = 0.d0 ! no se usa en los c'alculos
        by(1)     =-1.d0
        cy(1)     = 1.d0
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
  !AQUI PARALELIZABLE
        do jj = 1, ny
           
           tt(ii,jj,1) = ty(jj)
           
        end do
        !
     end do barrido_x
     !
     ! Criterio de convergencia
     !
  !AQUI PARALELIZABLE

     residuo = 0.d0

  !$omp parallel do private(ii,jj,id_thread) reduction(+:residuo) shared(tt)
     do ii = 1, nx
        id_thread = omp_get_thread_num()
        !write(*,*) "el contador ", ii, " desde el hilo, ", id_thread
        do jj = 1, ny
           residuo = residuo + (tt(ii,jj,1)-tt(ii,jj,2))*(tt(ii,jj,1)-tt(ii,jj,2))
        end do
     end do
 !$omp end parallel do


     !
     residuo = sqrt(residuo)
     !
     ! write(*,*) "DEBUG: ", iter, residuo
     !
     if( residuo < tolerancia )exit
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

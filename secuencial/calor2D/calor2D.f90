Program Calor2D
  !
  Implicit none
  !
  ! Declaracion de variables
  !
  ! Iteradores y tamaño del problema
  !
  integer :: ii, jj, iter
  integer, parameter :: nx = 60, ny = 30 !, itermax=1000
  !
  ! Variables del dominio computacional
  !
  double precision :: lx, ly, deltax, deltay, max_value_dif
  !
  ! Variables del problema fisico
  !
  double precision :: xx(nx), yy(ny)  ! variables de la malla
  double precision :: tt(nx,ny)       ! vector de incognitas
  double precision :: ct(nx,ny)       ! copia del vector de incognnitas
  double precision :: uu(nx,ny)		  ! Diferencia entre tt y ct
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
  ! Dominio computacional
  !
  lx      = 10.d0
  deltax  = lx/nx
  ly      = 5.d0
  deltay  = ly/ny
  max_value_dif = 0.d0 ! Esta variable será la el maximo de la diferencia
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
  tt(:,:) = 0.d0
  ct(:,:) = 0.d0
  uu(:,:) = 0.d0
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
  bucle_iteraciones: do while(1 == 1)
  !bucle_iteraciones: do iter = 1, itermax
     !
     ct(:,:) = tt(:,:)
     !
     barrido_y: do jj = 2, ny-1

        ensambla_tri_x: do ii = 2, nx-1

           ax(ii) = 1.d0/(deltax*deltax)
           bx(ii) =-2.d0*(1.d0/(deltax*deltax)+ 1.d0/(deltay*deltay))
           cx(ii) = 1.d0/(deltax*deltax)
           rx(ii) =-1.d0/(deltay*deltay)*tt(ii,jj-1)-1.d0/(deltay*deltay)*tt(ii,jj+1)
           
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
           
           tt(ii,jj) = tx(ii)
           
        end do
        !
     end do barrido_y

     barrido_x: do ii = 2, nx-1

        ensambla_tri_y: do jj = 2, ny-1

           ay(jj) = 1.d0/(deltay*deltay)
           by(jj) =-2.d0*(1.d0/(deltax*deltax)+ 1.d0/(deltay*deltay))
           cy(jj) = 1.d0/(deltay*deltay)
           ry(jj) =-1.d0/(deltax*deltax)*tt(ii-1,jj)-1.d0/(deltax*deltax)*tt(ii+1,jj)
           
        end do ensambla_tri_y
        !
        ! Impone cond. frontera
        !
        ay(1)     = 0.d0 ! no se usa en los c'alculos
        by(1)     = 1.d0
        cy(1)     = 0.d0
        ry(1)     = cfy(ii,1)
        !
        ay(ny)    = -1.d0 
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
           
           tt(ii,jj) = ty(jj)
           
        end do
        !
     end do barrido_x
     !
     !
     !Criterio de convergencia
     uu = tt - ct
     !
     uu = uu * uu
     max_value_dif = maxval(uu)
     if(max_value_dif < 1E-10) exit
     
     
  !end do bucle_iteraciones
  end do bucle_iteraciones
  !
  do jj = 1, ny
     do ii = 1, nx
        write(*,*) (ii-1)*deltax, (jj-1)*deltay, tt(ii,jj)
     end do
     write(*,*) ' '
  end do
  !
end Program Calor2D

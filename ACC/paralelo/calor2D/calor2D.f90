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
  double precision :: xx(nx), yy(ny)
  double precision :: tt(nx,ny,2)
  double precision :: resid_tt(nx,ny)
  double precision :: tx(nx), ty(ny)
  double precision :: cfx(ny,2), cfy(nx,2)
  !
  ! Arreglos 1D de tamaño nx*ny que contienen TODAS las tridiagonales
  ! del barrido al mismo tiempo. Indexacion plana:
  !   barrido en y -> idx = (jj-1)*nx + ii   (ny sistemas de tamaño nx)
  !   barrido en x -> idx = (ii-1)*ny + jj   (nx sistemas de tamaño ny)
  !
  double precision :: a(nx*ny), b(nx*ny), c(nx*ny), r(nx*ny)
  !
  character(48) :: archivo
  !
  tolerancia = 1d-4
  !
  lx     = 10.d0
  deltax = lx/nx
  ly     =  5.d0
  deltay = ly/ny
  do jj = 1, ny
     yy(jj) = (jj-1)*deltay
  end do
  do ii = 1, nx
     xx(ii) = (ii-1)*deltax
  end do
  !
  a(:)      = 0.d0
  b(:)      = 0.d0
  c(:)      = 0.d0
  r(:)      = 0.d0
  tt(:,:,:) = 0.d0
  !
  do ii = 1, ny
     cfx(ii,1) = 1.d0
     cfx(ii,2) = 0.d0
  end do
  do jj = 1, nx
     cfy(jj,1) = 1.d0
     cfy(jj,2) = 0.d0
  end do
  !
  bucle_iteraciones: do iter = 1, itermax
     !
     !$omp parallel do
     do jj = 2, ny-1
        do ii = 2, nx-1
           tt(ii,jj,2) = tt(ii,jj,1)
        end do
     end do
     !$omp end parallel do
     !
     !---------------------------------------------------------------
     ! Barrido en y: ensamblamos LAS ny TRIDIAGONALES AL MISMO TIEMPO
     ! sobre los arreglos planos a, b, c, r usando idx = (jj-1)*nx + ii
     !---------------------------------------------------------------
     !
     !$omp parallel do default(none) &
     !$omp shared(a, b, c, r, tt, cfx, deltax, deltay)
     ensambla_todas_y: do jj = 2, ny-1
        do ii = 2, nx-1
           a((jj-1)*nx + ii) = 1.d0/(deltax*deltax)
           b((jj-1)*nx + ii) =-2.d0*(1.d0/(deltax*deltax) + 1.d0/(deltay*deltay))
           c((jj-1)*nx + ii) = 1.d0/(deltax*deltax)
           r((jj-1)*nx + ii) =-1.d0/(deltay*deltay)*tt(ii,jj-1,1) &
                              -1.d0/(deltay*deltay)*tt(ii,jj+1,1)
        end do
        !
        ! Cond. de frontera del sistema jj
        !
        a((jj-1)*nx + 1)  = 0.d0
        b((jj-1)*nx + 1)  = 1.d0
        c((jj-1)*nx + 1)  = 0.d0
        r((jj-1)*nx + 1)  = cfx(jj,1)
        !
        a((jj-1)*nx + nx) = 0.d0
        b((jj-1)*nx + nx) = 1.d0
        c((jj-1)*nx + nx) = 0.d0
        r((jj-1)*nx + nx) = cfx(jj,2)
     end do ensambla_todas_y
     !$omp end parallel do
     !
     !$omp parallel do default(none) &
     !$omp shared(a, b, c, r, tt) &
     !$omp private(tx, ii)
     resuelve_todas_y: do jj = 2, ny-1
        call tri( a((jj-1)*nx+1 : jj*nx), &
                  b((jj-1)*nx+1 : jj*nx), &
                  c((jj-1)*nx+1 : jj*nx), &
                  r((jj-1)*nx+1 : jj*nx), &
                  tx, nx )
        do ii = 1, nx
           tt(ii,jj,1) = tx(ii)
        end do
     end do resuelve_todas_y
     !$omp end parallel do
     !
     !---------------------------------------------------------------
     ! Barrido en x: ensamblamos LAS nx TRIDIAGONALES AL MISMO TIEMPO
     ! sobre los mismos arreglos planos, idx = (ii-1)*ny + jj
     !---------------------------------------------------------------
     !
     !$omp parallel do default(none) &
     !$omp shared(a, b, c, r, tt, cfy, deltax, deltay)
     ensambla_todas_x: do ii = 2, nx-1
        do jj = 2, ny-1
           a((ii-1)*ny + jj) = 1.d0/(deltay*deltay)
           b((ii-1)*ny + jj) =-2.d0*(1.d0/(deltax*deltax) + 1.d0/(deltay*deltay))
           c((ii-1)*ny + jj) = 1.d0/(deltay*deltay)
           r((ii-1)*ny + jj) =-1.d0/(deltax*deltax)*tt(ii-1,jj,1) &
                              -1.d0/(deltax*deltax)*tt(ii+1,jj,1)
        end do
        !
        ! Cond. de frontera del sistema ii
        !
        a((ii-1)*ny + 1)  = 0.d0
        b((ii-1)*ny + 1)  = 1.d0
        c((ii-1)*ny + 1)  = 0.d0
        r((ii-1)*ny + 1)  = cfy(ii,1)
        !
        a((ii-1)*ny + ny) =-1.d0
        b((ii-1)*ny + ny) = 1.d0
        c((ii-1)*ny + ny) = 0.d0
        r((ii-1)*ny + ny) = cfy(ii,2)
     end do ensambla_todas_x
     !$omp end parallel do
     !
     !$omp parallel do default(none) &
     !$omp shared(a, b, c, r, tt) &
     !$omp private(ty, jj)
     resuelve_todas_x: do ii = 2, nx-1
        call tri( a((ii-1)*ny+1 : ii*ny), &
                  b((ii-1)*ny+1 : ii*ny), &
                  c((ii-1)*ny+1 : ii*ny), &
                  r((ii-1)*ny+1 : ii*ny), &
                  ty, ny )
        do jj = 1, ny
           tt(ii,jj,1) = ty(jj)
        end do
     end do resuelve_todas_x
     !$omp end parallel do
     !
     residuo = 0.d0
     !$omp parallel do reduction(+:residuo)
     do ii = 2, nx-1
        do jj = 2, ny-1
           residuo = residuo + (tt(ii,jj,1)-tt(ii,jj,2))*(tt(ii,jj,1)-tt(ii,jj,2))
        end do
     end do
     !$omp end parallel do
     !
     residuo = sqrt(residuo)
     !
  end do bucle_iteraciones
  !
  write(*,*) "Convergencia en ", iter, " iteraciones"
  !
  archivo = 'salida.vtk'
  !
end Program Calor2D

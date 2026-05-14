Program Calor2D
  !
  use omp_lib
  !
  use utiles, only : nx, ny, itermax
  !
  implicit none
  !
  integer :: ii, jj, iter
  !
  double precision :: lx, ly, deltax, deltay
  double precision :: inv_dx2, inv_dy2
  double precision :: residuo, tolerancia
  !
  double precision :: xx(nx), yy(ny)
  double precision :: tt(nx,ny,2)
  double precision :: tt_sweep(nx,ny)
  double precision :: tx(nx), ty(ny)
  double precision :: cfx(ny,2), cfy(nx,2)
  !
  ! Work arrays for the fused row/column algorithm. Each OpenMP worker gets a
  ! private copy, so every tridiagonal is assembled and solved locally.
  !
  double precision :: ax(nx), bx(nx), cx(nx), rx(nx)
  double precision :: ay(ny), by(ny), cy(ny), ry(ny)
  !
  tolerancia = 1d-4
  !
  lx      = 10.d0
  deltax  = lx/nx
  ly      = 5.d0
  deltay  = ly/ny
  inv_dx2 = 1.d0/(deltax*deltax)
  inv_dy2 = 1.d0/(deltay*deltay)
  !
  do jj = 1, ny
     yy(jj) = (jj-1)*deltay
  end do
  do ii = 1, nx
     xx(ii) = (ii-1)*deltax
  end do
  !
  tt(:,:,:)   = 0.d0
  tt_sweep(:,:) = 0.d0
  !
  do jj = 1, ny
     cfx(jj,1) = 1.d0
     cfx(jj,2) = 0.d0
  end do
  do ii = 1, nx
     cfy(ii,1) = 1.d0
     cfy(ii,2) = 0.d0
  end do
  !
  bucle_iteraciones: do iter = 1, itermax
     !
     !$omp parallel do default(none) shared(tt) private(ii,jj)
     do jj = 1, ny
        do ii = 1, nx
           tt(ii,jj,2) = tt(ii,jj,1)
        end do
     end do
     !$omp end parallel do
     !
     ! Snapshot for the y sweep. Without this copy, a row solved by one thread
     ! could read a neighbor row already updated by another thread.
     !
     !$omp parallel do default(none) shared(tt, tt_sweep) private(ii,jj)
     do jj = 1, ny
        do ii = 1, nx
           tt_sweep(ii,jj) = tt(ii,jj,1)
        end do
     end do
     !$omp end parallel do
     !
     !$omp parallel do default(none) &
     !$omp shared(tt, tt_sweep, cfx, inv_dx2, inv_dy2) &
     !$omp private(ax, bx, cx, rx, tx, ii)
     barrido_y_por_fila: do jj = 2, ny-1
        ax(1) = 0.d0
        bx(1) = 1.d0
        cx(1) = 0.d0
        rx(1) = cfx(jj,1)
        !
        do ii = 2, nx-1
           ax(ii) = inv_dx2
           bx(ii) = -2.d0*(inv_dx2 + inv_dy2)
           cx(ii) = inv_dx2
           rx(ii) = -inv_dy2*tt_sweep(ii,jj-1) &
                    -inv_dy2*tt_sweep(ii,jj+1)
        end do
        !
        ax(nx) = 0.d0
        bx(nx) = 1.d0
        cx(nx) = 0.d0
        rx(nx) = cfx(jj,2)
        !
        call tri(ax, bx, cx, rx, tx, nx)
        !
        do ii = 1, nx
           tt(ii,jj,1) = tx(ii)
        end do
     end do barrido_y_por_fila
     !$omp end parallel do
     !
     ! Snapshot for the x sweep, after the y sweep has finished.
     !
     !$omp parallel do default(none) shared(tt, tt_sweep) private(ii,jj)
     do jj = 1, ny
        do ii = 1, nx
           tt_sweep(ii,jj) = tt(ii,jj,1)
        end do
     end do
     !$omp end parallel do
     !
     !$omp parallel do default(none) &
     !$omp shared(tt, tt_sweep, cfy, inv_dx2, inv_dy2) &
     !$omp private(ay, by, cy, ry, ty, jj)
     barrido_x_por_columna: do ii = 2, nx-1
        ay(1) = 0.d0
        by(1) = 1.d0
        cy(1) = 0.d0
        ry(1) = cfy(ii,1)
        !
        do jj = 2, ny-1
           ay(jj) = inv_dy2
           by(jj) = -2.d0*(inv_dx2 + inv_dy2)
           cy(jj) = inv_dy2
           ry(jj) = -inv_dx2*tt_sweep(ii-1,jj) &
                    -inv_dx2*tt_sweep(ii+1,jj)
        end do
        !
        ay(ny) = -1.d0
        by(ny) = 1.d0
        cy(ny) = 0.d0
        ry(ny) = cfy(ii,2)
        !
        call tri(ay, by, cy, ry, ty, ny)
        !
        do jj = 1, ny
           tt(ii,jj,1) = ty(jj)
        end do
     end do barrido_x_por_columna
     !$omp end parallel do
     !
     residuo = 0.d0
     !$omp parallel do default(none) shared(tt) private(ii,jj) reduction(+:residuo)
     do jj = 2, ny-1
        do ii = 2, nx-1
           residuo = residuo + (tt(ii,jj,1)-tt(ii,jj,2)) &
                    *(tt(ii,jj,1)-tt(ii,jj,2))
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
end Program Calor2D

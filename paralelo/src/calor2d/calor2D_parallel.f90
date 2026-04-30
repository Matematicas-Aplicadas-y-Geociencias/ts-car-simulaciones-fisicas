program Calor2D
  use omp_lib
  use calor2D_utils
  implicit none

  integer, parameter :: nx = 480, ny = 240, itermax = 10000
  integer :: ii, jj, iter

  double precision :: lx, ly, deltax, deltay
  double precision :: inv_dx2, inv_dy2
  double precision :: residuo, tolerancia, tolerancia2

  double precision, allocatable :: tt_old(:,:), tt_mid(:,:), tt_new(:,:)
  double precision, allocatable :: cfx(:,:), cfy(:,:)

  double precision :: ax(nx), bx(nx), cx(nx), rx(nx), tx(nx)
  double precision :: ay(ny), by(ny), cy(ny), ry(ny), ty(ny)

  !--------------------------------------------
  ! Parametros
  !--------------------------------------------
  allocate(tt_old(nx,ny), tt_mid(nx,ny), tt_new(nx,ny))
  allocate(cfx(ny,2), cfy(nx,2))

  tolerancia = 1.d-3
  tolerancia2 = tolerancia*tolerancia

  lx     = 10.d0
  ly     = 5.d0
  deltax = lx / nx
  deltay = ly / ny
  inv_dx2 = 1.d0/(deltax*deltax)
  inv_dy2 = 1.d0/(deltay*deltay)

  tt_old = 0.d0
  tt_mid = 0.d0
  tt_new = 0.d0

  !--------------------------------------------
  ! Condiciones de frontera en x
  !--------------------------------------------
  do jj = 1, ny
     cfx(jj,1) = 1.d0
     cfx(jj,2) = 0.d0
  end do

  !--------------------------------------------
  ! Condiciones de frontera en y
  !--------------------------------------------
  do ii = 1, nx
     cfy(ii,1) = 1.d0
     cfy(ii,2) = 0.d0
  end do

  !--------------------------------------------
  ! Iteraciones
  !--------------------------------------------
  do iter = 1, itermax

     !=========================================
     ! Barrido en y:
     ! Cada linea en x se resuelve independientemente
     ! leyendo solamente tt_old y escribiendo en tt_mid
     !=========================================

     !$omp parallel do default(none) &
     !$omp shared(tt_old,tt_mid) &
     !$omp private(ii)
     do ii = 1, nx
        tt_mid(ii,1) = tt_old(ii,1)
        tt_mid(ii,ny) = tt_old(ii,ny)
     end do
     !$omp end parallel do

     !$omp parallel do default(none) &
     !$omp shared(inv_dx2,inv_dy2,tt_old,tt_mid,cfx) &
     !$omp private(ax,bx,cx,rx,tx,ii)
     do jj = 2, ny-1

        do ii = 2, nx-1
           ax(ii) = inv_dx2
           bx(ii) = -2.d0*(inv_dx2 + inv_dy2)
           cx(ii) = inv_dx2
           rx(ii) = -inv_dy2*tt_old(ii,jj-1) &
                    -inv_dy2*tt_old(ii,jj+1)
        end do

        ax(1)  = 0.d0
        bx(1)  = 1.d0
        cx(1)  = 0.d0
        rx(1)  = cfx(jj,1)

        ax(nx) = 0.d0
        bx(nx) = 1.d0
        cx(nx) = 0.d0
        rx(nx) = cfx(jj,2)

        call tri(ax, bx, cx, rx, tx, nx)

        do ii = 1, nx
           tt_mid(ii,jj) = tx(ii)
        end do

     end do
     !$omp end parallel do

     !=========================================
     ! Barrido en x:
     ! Cada columna en y se resuelve independientemente
     ! leyendo solamente de tt_mid y escribiendo en tt_new
     !=========================================

     !$omp parallel do default(none) &
     !$omp shared(tt_mid,tt_new) &
     !$omp private(jj)
     do jj = 1, ny
        tt_new(1,jj) = tt_mid(1,jj)
        tt_new(nx,jj) = tt_mid(nx,jj)
     end do
     !$omp end parallel do

     !$omp parallel do default(none) &
     !$omp shared(inv_dx2,inv_dy2,tt_mid,tt_new,cfy) &
     !$omp private(ay,by,cy,ry,ty,jj)
     do ii = 2, nx-1

        do jj = 2, ny-1
           ay(jj) = inv_dy2
           by(jj) = -2.d0*(inv_dx2 + inv_dy2)
           cy(jj) = inv_dy2
           ry(jj) = -inv_dx2*tt_mid(ii-1,jj) &
                    -inv_dx2*tt_mid(ii+1,jj)
        end do

        ay(1)  = 0.d0
      by(1)  = 1.d0
      cy(1)  = 0.d0
        ry(1)  = cfy(ii,1)

        ay(ny) = -1.d0
        by(ny) = 1.d0
        cy(ny) = 0.d0
        ry(ny) = cfy(ii,2)

        call tri(ay, by, cy, ry, ty, ny)

        do jj = 1, ny
           tt_new(ii,jj) = ty(jj)
        end do

     end do
     !$omp end parallel do

     !=========================================
     ! Residuo
     !=========================================
     call calc_residuo2(tt_new, tt_old, nx, ny, residuo)

   ! El corte por tolerancia queda desactivado para ejecutar siempre itermax.
   ! if (residuo < tolerancia2) exit

     !-----------------------------------------
     ! tt_old = tt_new
     !-----------------------------------------

     !$omp parallel do default(none) &
     !$omp shared(tt_new,tt_old) &
     !$omp private(ii,jj), collapse(2)
     do ii = 1, nx
        do jj = 1, ny
           tt_old(ii,jj) = tt_new(ii,jj)
        end do
     end do
     !$omp end parallel do

  end do

  write(*,*) 'Convergencia en ', iter, ' iteraciones'

  !do jj = 1, ny
   !  do ii = 1, nx
   !     write(*,*) (ii-1)*deltax, (jj-1)*deltay, tt_new(ii,jj)
    ! end do
     !write(*,*) ' '
  !end do

end program Calor2D

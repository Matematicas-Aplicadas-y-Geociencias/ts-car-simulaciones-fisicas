#ifndef NX
#define NX 480
#endif
#ifndef NY
#define NY 240
#endif
#ifndef ITERMAX
#define ITERMAX 10000
#endif

program Calor2D_ACC
  use calor2D_acc_utils, only : tri
  implicit none

  integer, parameter :: nx = NX, ny = NY, itermax = ITERMAX
  integer :: ii, jj, iter

  double precision :: lx, ly, deltax, deltay
  double precision :: inv_dx2, inv_dy2
  double precision :: residuo, checksum

  double precision, allocatable :: tt_old(:,:), tt_mid(:,:), tt_new(:,:)
  double precision, allocatable :: cfx(:,:), cfy(:,:)

  double precision :: ax(nx), bx(nx), cx(nx), rx(nx), tx(nx)
  double precision :: ay(ny), by(ny), cy(ny), ry(ny), ty(ny)

  allocate(tt_old(nx,ny), tt_mid(nx,ny), tt_new(nx,ny))
  allocate(cfx(ny,2), cfy(nx,2))

  lx = 10.d0
  ly = 5.d0
  deltax = lx/nx
  deltay = ly/ny
  inv_dx2 = 1.d0/(deltax*deltax)
  inv_dy2 = 1.d0/(deltay*deltay)

  tt_old(:,:) = 0.d0
  tt_mid(:,:) = 0.d0
  tt_new(:,:) = 0.d0

  do jj = 1, ny
     cfx(jj,1) = 1.d0
     cfx(jj,2) = 0.d0
  end do

  do ii = 1, nx
     cfy(ii,1) = 1.d0
     cfy(ii,2) = 0.d0
  end do

  !$acc data copyin(cfx, cfy) copy(tt_old, tt_mid, tt_new)
  do iter = 1, itermax

     !$acc parallel loop present(tt_old, tt_mid)
     do ii = 1, nx
        tt_mid(ii,1) = tt_old(ii,1)
        tt_mid(ii,ny) = tt_old(ii,ny)
     end do
     !$acc end parallel loop

     !$acc parallel loop gang present(tt_old, tt_mid, cfx) &
     !$acc private(ax, bx, cx, rx, tx)
     do jj = 2, ny-1
        !$acc loop seq
        do ii = 2, nx-1
           ax(ii) = inv_dx2
           bx(ii) = -2.d0*(inv_dx2 + inv_dy2)
           cx(ii) = inv_dx2
           rx(ii) = -inv_dy2*tt_old(ii,jj-1) - inv_dy2*tt_old(ii,jj+1)
        end do

        ax(1) = 0.d0
        bx(1) = 1.d0
        cx(1) = 0.d0
        rx(1) = cfx(jj,1)

        ax(nx) = 0.d0
        bx(nx) = 1.d0
        cx(nx) = 0.d0
        rx(nx) = cfx(jj,2)

        call tri(ax, bx, cx, rx, tx, nx)

        !$acc loop seq
        do ii = 1, nx
           tt_mid(ii,jj) = tx(ii)
        end do
     end do
     !$acc end parallel loop

     !$acc parallel loop present(tt_mid, tt_new)
     do jj = 1, ny
        tt_new(1,jj) = tt_mid(1,jj)
        tt_new(nx,jj) = tt_mid(nx,jj)
     end do
     !$acc end parallel loop

     !$acc parallel loop gang present(tt_mid, tt_new, cfy) &
     !$acc private(ay, by, cy, ry, ty)
     do ii = 2, nx-1
        !$acc loop seq
        do jj = 2, ny-1
           ay(jj) = inv_dy2
           by(jj) = -2.d0*(inv_dx2 + inv_dy2)
           cy(jj) = inv_dy2
           ry(jj) = -inv_dx2*tt_mid(ii-1,jj) - inv_dx2*tt_mid(ii+1,jj)
        end do

        ay(1) = 0.d0
        by(1) = 1.d0
        cy(1) = 0.d0
        ry(1) = cfy(ii,1)

        ay(ny) = -1.d0
        by(ny) = 1.d0
        cy(ny) = 0.d0
        ry(ny) = cfy(ii,2)

        call tri(ay, by, cy, ry, ty, ny)

        !$acc loop seq
        do jj = 1, ny
           tt_new(ii,jj) = ty(jj)
        end do
     end do
     !$acc end parallel loop

     residuo = 0.d0
     !$acc parallel loop collapse(2) reduction(+:residuo) present(tt_new, tt_old)
     do jj = 1, ny
        do ii = 1, nx
           residuo = residuo + (tt_new(ii,jj) - tt_old(ii,jj)) &
                              *(tt_new(ii,jj) - tt_old(ii,jj))
        end do
     end do
     !$acc end parallel loop
     residuo = sqrt(residuo)

     !$acc parallel loop collapse(2) present(tt_new, tt_old)
     do jj = 1, ny
        do ii = 1, nx
           tt_old(ii,jj) = tt_new(ii,jj)
        end do
     end do
     !$acc end parallel loop
  end do

  checksum = 0.d0
  !$acc parallel loop collapse(2) reduction(+:checksum) present(tt_new)
  do jj = 1, ny
     do ii = 1, nx
        checksum = checksum + tt_new(ii,jj)
     end do
  end do
  !$acc end parallel loop
  !$acc end data

  write(*,'(A,I0,A,I0,A,I0)') 'nx=', nx, ' ny=', ny, ' itermax=', itermax
  write(*,'(A,I0)') 'iteraciones=', itermax
  write(*,'(A,ES24.16)') 'residuo=', residuo
  write(*,'(A,ES24.16)') 'checksum=', checksum

  do jj = 1, ny
     do ii = 1, nx
        write(*,'(3(ES24.16,1X))') (ii-1)*deltax, (jj-1)*deltay, tt_new(ii,jj)
     end do
     write(*,*) ' '
  end do

  deallocate(tt_old, tt_mid, tt_new)
  deallocate(cfx, cfy)
end program Calor2D_ACC

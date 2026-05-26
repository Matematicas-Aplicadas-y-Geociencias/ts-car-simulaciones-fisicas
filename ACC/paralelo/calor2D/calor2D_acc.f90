#ifndef NX
#define NX 300
#endif
#ifndef NY
#define NY 150
#endif
#ifndef ITERMAX
#define ITERMAX 20000
#endif

program Calor2D_ACC
  use calor2D_acc_utils, only : tri_factor, tri_solve
  implicit none

  integer, parameter :: nx = NX, ny = NY, itermax = ITERMAX
  integer :: ii, jj, iter

  double precision :: lx, ly, deltax, deltay
  double precision :: inv_dx2, inv_dy2
  double precision :: residuo, tolerancia, tolerancia2, checksum

  double precision :: tt_old(nx, ny), tt_mid(nx, ny), tt_new(nx, ny)
  double precision :: cfx(ny, 2), cfy(nx, 2)
  double precision :: rx(nx), tx(nx), ry(ny), ty(ny)

  ! Coeficientes constantes y factorizacion Thomas por direccion.
  double precision :: ax(nx), bx_base(nx), cx(nx), cpx(nx), denx(nx)
  double precision :: ay(ny), by_base(ny), cy(ny), cpy(ny), deny(ny)

  tolerancia = 1.d-3
  tolerancia2 = tolerancia * tolerancia

  lx = 10.d0
  ly = 5.d0
  deltax = lx / (nx - 1)
  deltay = ly / (ny - 1)
  inv_dx2 = 1.d0 / (deltax * deltax)
  inv_dy2 = 1.d0 / (deltay * deltay)

  tt_old = 0.d0
  tt_mid = 0.d0
  tt_new = 0.d0
  ax = 0.d0
  bx_base = 0.d0
  cx = 0.d0
  ay = 0.d0
  by_base = 0.d0
  cy = 0.d0

  do ii = 2, nx - 1
     ax(ii) = inv_dx2
     bx_base(ii) = -2.d0 * (inv_dx2 + inv_dy2)
     cx(ii) = inv_dx2
  end do
  ax(1) = 0.d0
  bx_base(1) = 1.d0
  cx(1) = 0.d0
  ax(nx) = 0.d0
  bx_base(nx) = 1.d0
  cx(nx) = 0.d0

  do jj = 2, ny - 1
     ay(jj) = inv_dy2
     by_base(jj) = -2.d0 * (inv_dx2 + inv_dy2)
     cy(jj) = inv_dy2
  end do
  ay(1) = 0.d0
  by_base(1) = -1.d0
  cy(1) = 1.d0
  ay(ny) = 0.d0
  by_base(ny) = 1.d0
  cy(ny) = 0.d0

  call tri_factor(ax, bx_base, cx, cpx, denx, nx)
  call tri_factor(ay, by_base, cy, cpy, deny, ny)

  do jj = 1, ny
     cfx(jj, 1) = 1.d0
     cfx(jj, 2) = 0.d0
  end do

  do ii = 1, nx
     cfy(ii, 1) = 0.d0
     cfy(ii, 2) = 1.d0
  end do

  !$acc data copy(tt_old, tt_mid, tt_new) &
  !$acc      copyin(cfx, cfy, inv_dx2, inv_dy2, ax, cpx, denx, ay, cpy, deny)
  do iter = 1, itermax

     !$acc parallel loop present(tt_old, tt_mid)
     do ii = 1, nx
        tt_mid(ii, 1) = tt_old(ii, 1)
        tt_mid(ii, ny) = tt_old(ii, ny)
     end do
     !$acc end parallel loop

     ! Barrido en y: armar RHS por fila y resolver con factores constantes.
     !$acc parallel loop gang present(tt_old, tt_mid, cfx, inv_dy2, ax, cpx, denx) &
     !$acc      private(rx, tx)
     do jj = 2, ny - 1
        !$acc loop seq
        do ii = 2, nx - 1
           rx(ii) = -inv_dy2 * tt_old(ii, jj - 1) - inv_dy2 * tt_old(ii, jj + 1)
        end do
        rx(1) = cfx(jj, 1)
        rx(nx) = cfx(jj, 2)

        call tri_solve(ax, cpx, denx, rx, tx, nx)

        !$acc loop seq
        do ii = 1, nx
           tt_mid(ii, jj) = tx(ii)
        end do
     end do
     !$acc end parallel loop

     !$acc parallel loop present(tt_mid, tt_new)
     do jj = 1, ny
        tt_new(1, jj) = tt_mid(1, jj)
        tt_new(nx, jj) = tt_mid(nx, jj)
     end do
     !$acc end parallel loop

     ! Barrido en x: armar RHS por columna y resolver con factores constantes.
     !$acc parallel loop gang present(tt_mid, tt_new, cfy, inv_dx2, ay, cpy, deny) &
     !$acc      private(ry, ty)
     do ii = 2, nx - 1
        !$acc loop seq
        do jj = 2, ny - 1
           ry(jj) = -inv_dx2 * tt_mid(ii - 1, jj) - inv_dx2 * tt_mid(ii + 1, jj)
        end do
        ry(1) = cfy(ii, 1)
        ry(ny) = cfy(ii, 2)

        call tri_solve(ay, cpy, deny, ry, ty, ny)

        !$acc loop seq
        do jj = 1, ny
           tt_new(ii, jj) = ty(jj)
        end do
     end do
     !$acc end parallel loop

     residuo = 0.d0
     !$acc parallel loop collapse(2) reduction(+:residuo) present(tt_new, tt_old)
     do jj = 1, ny
        do ii = 1, nx
           residuo = residuo + (tt_new(ii, jj) - tt_old(ii, jj)) &
                * (tt_new(ii, jj) - tt_old(ii, jj))
        end do
     end do
     !$acc end parallel loop

     if (residuo < tolerancia2) exit

     !$acc parallel loop collapse(2) present(tt_old, tt_new)
     do jj = 1, ny
        do ii = 1, nx
           tt_old(ii, jj) = tt_new(ii, jj)
        end do
     end do
     !$acc end parallel loop

  end do

  checksum = 0.d0
  !$acc parallel loop collapse(2) reduction(+:checksum) present(tt_new)
  do jj = 1, ny
     do ii = 1, nx
        checksum = checksum + tt_new(ii, jj)
     end do
  end do
  !$acc end parallel loop
  !$acc end data

  write(*, '(A,I0,A,I0,A,I0)') 'nx=', nx, ' ny=', ny, ' itermax=', itermax
  write(*, '(A,I0)') 'iteraciones=', iter
  write(*, '(A,ES24.16)') 'residuo=', residuo
  write(*, '(A,ES24.16)') 'checksum=', checksum

  block
     character(len=128) :: dump_env
     integer :: dump_len

     call get_environment_variable('CALOR2D_DUMP_MESH', dump_env, length=dump_len)
     if (dump_len > 0) then
        open(unit=101, file='resultados/tablas/resultado_malla.dat', status='replace', action='write')
        do jj = 1, ny
           do ii = 1, nx
              write(101, *) (ii - 1) * deltax, (jj - 1) * deltay, tt_new(ii, jj)
           end do
        end do
        close(101)
     end if
  end block

end program Calor2D_ACC

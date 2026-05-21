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
  use calor2D_acc_utils, only : indicex, indicey, tri
  implicit none

  integer, parameter :: nx = NX, ny = NY, itermax = ITERMAX
  integer :: ii, jj, iter

  double precision :: lx, ly, deltax, deltay
  double precision :: inv_dx2, inv_dy2
  double precision :: residuo, tolerancia, tolerancia2, checksum

  double precision :: tt(nx, ny, 2)
  double precision :: cfx(ny, 2), cfy(nx, 2)
  double precision :: tx(nx), ty(ny)

  ! Coeficientes y segundo miembro planos (todas las tridiagonales de un barrido)
  double precision :: aa(nx * ny), bb(nx * ny), cc(nx * ny), rr(nx * ny)

  tolerancia = 1.d-3
  tolerancia2 = tolerancia * tolerancia

  lx = 10.d0
  ly = 5.d0
  deltax = lx / (nx - 1)
  deltay = ly / (ny - 1)
  inv_dx2 = 1.d0 / (deltax * deltax)
  inv_dy2 = 1.d0 / (deltay * deltay)

  tt = 0.d0
  aa = 0.d0
  bb = 0.d0
  cc = 0.d0
  rr = 0.d0

  do jj = 1, ny
     cfx(jj, 1) = 1.d0
     cfx(jj, 2) = 0.d0
  end do

  do ii = 1, nx
     cfy(ii, 1) = 0.d0
     cfy(ii, 2) = 1.d0
  end do

  !$acc data copy(tt, aa, bb, cc, rr) copyin(cfx, cfy, inv_dx2, inv_dy2)
  do iter = 1, itermax

     !$acc parallel loop collapse(2) present(tt)
     do jj = 1, ny
        do ii = 1, nx
           tt(ii, jj, 2) = tt(ii, jj, 1)
        end do
     end do
     !$acc end parallel loop

     ! Barrido en y: ensamblar ny sistemas tridiagonales en aa,bb,cc,rr (indicex)
     !$acc parallel loop gang present(tt, aa, bb, cc, rr, cfx, inv_dx2, inv_dy2)
     do jj = 2, ny - 1
        !$acc loop seq
        do ii = 2, nx - 1
           aa(indicex(ii, jj)) = inv_dx2
           bb(indicex(ii, jj)) = -2.d0 * (inv_dx2 + inv_dy2)
           cc(indicex(ii, jj)) = inv_dx2
           rr(indicex(ii, jj)) = -inv_dy2 * tt(ii, jj - 1, 1) - inv_dy2 * tt(ii, jj + 1, 1)
        end do
        aa(indicex(1, jj)) = 0.d0
        bb(indicex(1, jj)) = 1.d0
        cc(indicex(1, jj)) = 0.d0
        rr(indicex(1, jj)) = cfx(jj, 1)
        aa(indicex(nx, jj)) = 0.d0
        bb(indicex(nx, jj)) = 1.d0
        cc(indicex(nx, jj)) = 0.d0
        rr(indicex(nx, jj)) = cfx(jj, 2)
     end do
     !$acc end parallel loop

     ! Resolver filas (tri destruye bb,rr del tramo; cada jj usa su slice)
     !$acc parallel loop gang present(aa, bb, cc, rr, tt) private(tx)
     do jj = 2, ny - 1
        call tri( &
             aa(indicex(1, jj):indicex(nx, jj)), &
             bb(indicex(1, jj):indicex(nx, jj)), &
             cc(indicex(1, jj):indicex(nx, jj)), &
             rr(indicex(1, jj):indicex(nx, jj)), &
             tx, nx)
        !$acc loop seq
        do ii = 1, nx
           tt(ii, jj, 1) = tx(ii)
        end do
     end do
     !$acc end parallel loop

     ! Barrido en x: ensamblar con indicey
     !$acc parallel loop gang present(tt, aa, bb, cc, rr, cfy, inv_dx2, inv_dy2)
     do ii = 2, nx - 1
        !$acc loop seq
        do jj = 2, ny - 1
           aa(indicey(ii, jj)) = inv_dy2
           bb(indicey(ii, jj)) = -2.d0 * (inv_dx2 + inv_dy2)
           cc(indicey(ii, jj)) = inv_dy2
           rr(indicey(ii, jj)) = -inv_dx2 * tt(ii - 1, jj, 1) - inv_dx2 * tt(ii + 1, jj, 1)
        end do
        aa(indicey(ii, 1)) = 0.d0
        bb(indicey(ii, 1)) = -1.d0
        cc(indicey(ii, 1)) = 1.d0
        rr(indicey(ii, 1)) = cfy(ii, 1)
        aa(indicey(ii, ny)) = 0.d0
        bb(indicey(ii, ny)) = 1.d0
        cc(indicey(ii, ny)) = 0.d0
        rr(indicey(ii, ny)) = cfy(ii, 2)
     end do
     !$acc end parallel loop

     !$acc parallel loop gang present(aa, bb, cc, rr, tt) private(ty)
     do ii = 2, nx - 1
        call tri( &
             aa(indicey(ii, 1):indicey(ii, ny)), &
             bb(indicey(ii, 1):indicey(ii, ny)), &
             cc(indicey(ii, 1):indicey(ii, ny)), &
             rr(indicey(ii, 1):indicey(ii, ny)), &
             ty, ny)
        !$acc loop seq
        do jj = 1, ny
           tt(ii, jj, 1) = ty(jj)
        end do
     end do
     !$acc end parallel loop

     residuo = 0.d0
     !$acc parallel loop collapse(2) reduction(+:residuo) present(tt)
     do jj = 1, ny
        do ii = 1, nx
           residuo = residuo + (tt(ii, jj, 1) - tt(ii, jj, 2)) &
                * (tt(ii, jj, 1) - tt(ii, jj, 2))
        end do
     end do
     !$acc end parallel loop

     if (residuo < tolerancia2) exit

  end do

  checksum = 0.d0
  !$acc parallel loop collapse(2) reduction(+:checksum) present(tt)
  do jj = 1, ny
     do ii = 1, nx
        checksum = checksum + tt(ii, jj, 1)
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
              write(101, *) (ii - 1) * deltax, (jj - 1) * deltay, tt(ii, jj, 1)
           end do
        end do
        close(101)
     end if
  end block

end program Calor2D_ACC

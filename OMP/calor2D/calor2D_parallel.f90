program Calor2D
  use omp_lib
  use calor2D_utils
  implicit none

  integer, parameter :: nx = 300, ny = 150, itermax = 20000
  integer :: ii, jj, iter

  double precision :: lx, ly, deltax, deltay
  double precision :: inv_dx2, inv_dy2
  double precision :: residuo, tolerancia, tolerancia2

  double precision, allocatable :: tt_old(:,:), tt_mid(:,:), tt_new(:,:)
  double precision, allocatable :: cfx(:,:), cfy(:,:)

  ! Coeficientes constantes y factorizaci\'on Thomas (compartidos, solo lectura)
  double precision :: ax(nx), cx(nx), bx_base(nx)
  double precision :: cpx(nx), denx(nx)
  double precision :: ay(ny), cy(ny), by_base(ny)
  double precision :: cpy(ny), deny(ny)

  ! Vectores de trabajo por hilo (private en cada parallel do)
  double precision :: rx(nx), tx(nx)
  double precision :: ry(ny), ty(ny)

  allocate(tt_old(nx, ny), tt_mid(nx, ny), tt_new(nx, ny))
  allocate(cfx(ny, 2), cfy(nx, 2))

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

  ! Coeficientes tridiagonales
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
     cfy(ii,1) = 0.d0
     cfy(ii,2) = 1.d0
  end do

  !-----------------------------------------------
  ! Iteraciones
  !-----------------------------------------------
  do iter = 1, itermax

   !-----------------------------------------------   
   ! 1. Barrido en y (líneas horizontales)
   !-----------------------------------------------
   ! Cada línea en x se resuelve en paralelo
   ! leyendo solamente tt_old y escribiendo en tt_mid

     !$omp parallel do default(none) &
     !$omp shared(tt_old, tt_mid) private(ii) schedule(static)
     do ii = 1, nx
        tt_mid(ii, 1) = tt_old(ii, 1)
        tt_mid(ii, ny) = tt_old(ii, ny)
     end do
     !$omp end parallel do

     !$omp parallel do default(none) &
     !$omp shared(inv_dy2, tt_old, tt_mid, cfx, ax, cpx, denx) &
     !$omp private(rx, tx, ii) schedule(static)
     do jj = 2, ny - 1

        do ii = 2, nx - 1
           rx(ii) = -inv_dy2 * tt_old(ii, jj - 1) - inv_dy2 * tt_old(ii, jj + 1)
        end do

        rx(1) = cfx(jj, 1)
        rx(nx) = cfx(jj, 2)

        call tri_solve(ax, cpx, denx, rx, tx, nx)

        do ii = 1, nx
           tt_mid(ii, jj) = tx(ii)
        end do

     end do
     !$omp end parallel do

     !-----------------------------------------------   
     ! 2. Barrido en x (líneas verticales)
     !-----------------------------------------------
     ! Cada línea en y se resuelve en paralelo
     ! leyendo solamente tt_mid y escribiendo en tt_new

     !$omp parallel do default(none) &
     !$omp shared(tt_mid, tt_new) private(jj) schedule(static)
     do jj = 1, ny
        tt_new(1, jj) = tt_mid(1, jj)
        tt_new(nx, jj) = tt_mid(nx, jj)
     end do
     !$omp end parallel do

     !$omp parallel do default(none) &
     !$omp shared(inv_dx2, tt_mid, tt_new, cfy, ay, cpy, deny) &
     !$omp private(ry, ty, jj) schedule(static)
     do ii = 2, nx - 1

        do jj = 2, ny - 1
           ry(jj) = -inv_dx2 * tt_mid(ii - 1, jj) - inv_dx2 * tt_mid(ii + 1, jj)
        end do

        ry(1) = cfy(ii, 1)
        ry(ny) = cfy(ii, 2)

        call tri_solve(ay, cpy, deny, ry, ty, ny)

        do jj = 1, ny
           tt_new(ii, jj) = ty(jj)
        end do

     end do
     !$omp end parallel do

     ! --------------------------------
     ! 3. Residuo
     ! --------------------------------
     ! Calcula el residuo entre tt_new y tt_old
     ! --------------------------------
     call calc_residuo2(tt_new, tt_old, nx, ny, residuo)
     ! --------------------------------
     ! 3.1. Criterio de convergencia
     ! --------------------------------
     ! Si el residuo es menor que la tolerancia, se sale del bucle
     ! --------------------------------   
     if (residuo < tolerancia2) then
        exit
     end if
     ! --------------------------------
     ! 4. Actualizaci\'on de tt_old
     ! --------------------------------
     ! Actualiza tt_old con tt_new
     ! --------------------------------
     !$omp parallel do default(none) &
     !$omp shared(tt_new, tt_old) private(ii, jj) collapse(2) schedule(static)
     do ii = 1, nx
        do jj = 1, ny
           tt_old(ii, jj) = tt_new(ii, jj)
        end do
     end do
     !$omp end parallel do

  end do

  write(*,*) 'Convergencia en ', iter, ' iteraciones'

end program Calor2D

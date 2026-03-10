program ids_a_n
  use omp_lib
  implicit none
  integer :: N, tid, S

  N = 12
  S = 0

  !$omp parallel private(tid) shared(N,S)
    tid = omp_get_thread_num()   ! 0..T-1

      !$omp atomic
      S = S + (tid + 1)

  !$omp end parallel

  print *, "Suma =", S
end program
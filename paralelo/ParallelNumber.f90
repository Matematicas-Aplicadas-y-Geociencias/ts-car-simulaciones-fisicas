program ids_a_n
  use omp_lib
  implicit none
  integer :: N, tid, S

  N = 13 ! N'umero N para sumar
  S = 0 ! Suma inicializada a 0

  ! num_threads(N) hace que el número de hilos sea igual a N
  !$omp parallel private(tid) reduction(+:S) num_threads(N)
    tid = omp_get_thread_num()
    S = tid + 1
  !$omp end parallel

  print *, "Suma =", S
end program ids_a_n
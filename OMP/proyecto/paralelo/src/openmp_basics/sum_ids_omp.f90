program ids_a_n
  
  use omp_lib
  
  implicit none
  integer :: N, S, i, tid
  

  ! Programa para sumar de 1 a N secuencialmente
  N = 13 ! N'umero N para sumar
  S = 0
  do i = 1, N
    S = S + i
  end do
  print *, "Suma =", S

  ! Programa para sumar de 1 a N utilizando OpenMP
  
  S = 0 ! Suma inicializada a 0

  ! num_threads(N) hace que el número de hilos sea igual a N
  !$omp parallel private(tid) reduction(+:S) num_threads(N)
    tid = omp_get_thread_num()
    S = tid + 1
  !$omp end parallel

  print *, "Suma =", S
end program ids_a_n
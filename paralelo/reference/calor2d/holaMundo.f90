Program holaMundoOpenMP
  !
  ! Cargamos el m'odulo de funciones de openmp
  !
  use omp_lib
  !
  ! Variables para describir la regi'on paralela
  !
  integer :: nthreads, id_thread
  !
  ! Este programa utiliza una regi'on paralela de OpenMP
  ! para escribir un mensaje en la pantalla
  !
  ! Escribimos mensaje fuera de la regi'on paralela
  write(*,*) "Hola Mundo fuera de la regi'on paralela"
  !
  ! Abrimos la regi'on paralela con una variable privada y otra compartida
  ! Se puede definir el n'umero de hilos en una regi'on paralela usando el
  ! comando export OMP_NUM_THREADS=N
  !
  ! En CPUs es muy bueno dar una gran cantidad de operaciones a cada hilo de
  ! c'omputo
  !
  !$omp parallel private(id_thread) shared(nthreads)
  !
  ! Utilizamos una funci'on de openmp para saber cu'antos threads tenemos
  nthreads = omp_get_num_threads()
  !
  ! Utilizamos una funci'on de openmp para saber cu'antos threads tenemos
  id_thread = omp_get_thread_num()
  !
  ! Escribimos el mensaje de n'umero de threads desde el hilo 2
  if ( id_thread == 2 ) then
     write(*,*) "Tenemos ", nthreads," threads disponibles "
  end if
  !
  write(*,*) "Hola Mundo desde la regi'on paralela con el id ", id_thread
  !
  ! Cerramos la regi'on paralela
  !
  !$omp end parallel
  !
  ! Escribimos mensaje fuera de la regi'on paralela
  write(*,*) "Hola Mundo fuera de la regi'on paralela 2"
  !
end Program holaMundoOpenMP

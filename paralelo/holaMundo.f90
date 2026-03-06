Program holaMundoOpenMP
  !
  !CARGAMOS MODULOS
  use omp_lib
  ! Este programa utiliza una regi'on paralela de OpenMP
  !# de hilos
  integer:: nthreads, id_threads
  ! para escribir un mensaje en la pantalla
  !En CPU es buena idea dar un gran número de operaciones a cada hilo.

  ! Escribimos mensaje fuera de la regi'on paralela
  write(*,*) "Hola Mundo fuera de la regi'on paralela"
  !
  ! Abrimos la regi'on paralela con una variable privada (omp parallel private (id_thread) y una compartida shared(ntrheads) 
  !
  !$omp parallel private(id_thread) shared(nthreads)
  ! podemos delimitar el número de hilos utilizables desde el inicio, pero hace más rígido al programa usando la variable de entorno export OMP_NUM_TRHEADS=4
  
       !Identificamos los hilos
  id_thread = omp_get_thread_num()
  !Usamos una función de openmp para saber cuandos hilos tenemos
   nthreads = omp_get_num_threads()
   if(id_thread == 2) then 
   	  write(*,*) "tenemos ", nthreads, "hilos disponibles"
   	  end if

  write(*,*) "Hola Mundo desde la región paralela con el id", id_thread
  !$omp end parallel
  !
  ! Cerramos la regi'on paralela
  !
  ! Escribimos mensaje fuera de la regi'on paralela
  write(*,*) "Hola Mundo fuera de la regi'on paralela 2"
end Program holaMundoOpenMP

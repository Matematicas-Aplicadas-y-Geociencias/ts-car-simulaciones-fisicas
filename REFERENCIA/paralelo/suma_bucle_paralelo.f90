Program suma_paralela
  !
  ! Este programa suma los pimeros N n'umeros usando una regi'on paralela
  !
  use omp_lib
  !
  implicit none
  !
  integer :: ii, suma, id_thread
  !
  suma = 0
  !
  ! Abrimos una regi\'on paralela donde el identificador de hilo ser'a privado
  ! y se realizar'a la suma de los primeros N n\'umeros usando un bucle paralelizado
  !
  !$omp parallel private(id_thread) reduction(+:suma) default(none)
  !
  id_thread = omp_get_thread_num()
  ! write(*, *) "id_thread: ", id_thread
  !
  ! Se abre un bucle paralelo, donde los \'indices del repetidor son privados por defecto
  ! pero no los declaramos expl'citamente (se encarga OpenMP). El n\'umero de iteraciones
  ! se reparte de la forma m\'as equitativa posible entre los hilos
  !
  !$omp do
  do ii = 1, 2048
     suma = suma + ii
     print*, "El \'indice es ii=", ii, " desde el thread ", id_thread
  end do
  !
  ! Se cierra el bucle paralelo, se garantiza que las variables compartidas est\'an actualizadas
  !
  !$omp end do
  print*, "El resultado de la suma es ", suma, " desde el thread ", id_thread
  !
  ! Cerramos la regi\'on la regi'on paralela. En este punto todas la copias
  ! privadas de variables se destruyen, y aquellas que est'an declaradas en la
  ! lista de reducci'on se acumulan de acuerdo al operador indicado
  !
  !$omp end parallel
  !
  write(*, *) suma
  !
end Program suma_paralela

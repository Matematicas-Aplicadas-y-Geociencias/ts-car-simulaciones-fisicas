Program suma_paralela
  !
  ! Este programa suma los pimeros N n'umeros usando una regi'on paralela
  !
  use omp_lib
  !
  implicit none
  !
  integer :: suma, id_thread
  !
  suma = 0
  !
  ! Abrimos una regi\'on paralela donde el identificador de hilo ser'a privado
  ! y acumularemos los valores para realizar la suma en una variable especial
  ! de tipo reducci'on
  !
  ! Las regiones paralelas admiten muchas clausulas en su definici'on, las
  ! m'as usuales son:
  ! private(lista), shared(lista), reduction(+,*,-: lista), default(lista)
  ! num_threads(n'umero)
  !
  !$omp parallel private(id_thread) reduction(+:suma) num_threads(65)
  !
  id_thread = omp_get_thread_num()
  ! write(*, *) "id_thread: ", id_thread
  suma = suma + id_thread
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


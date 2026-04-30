Program holaMundoOpenMP
  !
  ! Este programa utiliza una regi'on paralela de OpenMP
  ! para escribir un mensaje en la pantalla
  !
  ! Escribimos mensaje fuera de la regi'on paralela
  write(*,*) "Hola Mundo fuera de la regi'on paralela"
  !
  ! Abrimos la regi'on paralela
  !
  !$omp parallel
  write(*,*) "Hola Mundo"
  !$omp end parallel
  !
  ! Cerramos la regi'on paralela
  !
  ! Escribimos mensaje fuera de la regi'on paralela
  write(*,*) "Hola Mundo fuera de la regi'on paralela 2"
end Program holaMundoOpenMP

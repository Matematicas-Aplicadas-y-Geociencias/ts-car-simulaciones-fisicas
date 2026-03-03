Program holaMundoOpenMP
  !
  ! Este programa utiliza una regi'on paralela de OpenMP
  ! para escribir un mensaje en la pantalla
  !
  !$omp parallel
  write(*,*) "Hola Mundo"
  !$omp end parallel
  !
end Program holaMundoOpenMP

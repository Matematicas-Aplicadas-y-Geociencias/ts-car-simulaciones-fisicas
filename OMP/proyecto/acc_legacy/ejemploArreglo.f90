! Programa de ejemplo para uso de OpenACC
program ejemploArreglo

  use openacc
  implicit none

  integer, parameter :: dimen = 10000
  double precision :: arreglo(dimen)
  integer :: tt

  !$acc parallel loop
  do tt = 1, dimen
    arreglo(tt) = 2.0d0 * tt
  end do
  !$acc end parallel loop

  print *, "Elemento 13 de arreglo: ", arreglo(13)

end program ejemploArreglo

Program suma_paralela
use omp_lib

integer :: suma, id_thread

suma = 0
!$omp parallel private(id_thread) shared(suma)

id_thread = omp_get_thread_num()
write(*, *) "id_thread: ", id_thread
suma = suma + id_thread

!$omp end parallel

write(*, *) suma

end Program suma_paralela
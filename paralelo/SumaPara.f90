Program suma_paralela
use omp_lib
!
implicit none 
integer::suma,id_thread
!
suma=0
!abrimos una región paralela donde el identificador hilo será privado y acumularemos los valores para realizar la suma en una variable especial de tipo reducción
!Las regiones paralelas admiten muchas cláusulas en su definición, las más usuales son:
!private(lista), dhared(lista), reduction(+,*,-:lista), default(lista) y num_threads(#) define el numero especifico de hilos diferente a la de entorno 
!hay que tener cuidado default 
!$omp parallel private(id_thread) reduction(+:suma) num_threads(101)

id_thread = omp_get_thread_num()
!
suma = suma +id_thread

!Cerramos la región paralela. En este punto las copias privadas de variables se destruyen y aquellas que están declaradas en la lista de reducción se acumulan al operador indicado
!$omp end parallel 
write(*,*) suma
end Program suma_paralela

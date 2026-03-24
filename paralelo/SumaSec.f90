program suma_sec
    use omp_lib
    implicit none
    integer :: i, suma, id_thread
    
    suma=0
    do i = 1,64
    	suma =suma +i
    end do
    print*, "El resultado de la suma es:", suma
   
    !Ahora usaremos a paralelizar los bucles, esto es un nuevo constructor, el cual comparte trabajo, llamado "!$omp do" e igualmente tiene clausulas (variables). Así que lo que haremos es lo siguiente 
 
 !Abrimos la región paralela
  suma=0
   print*, "se resetea la variable suma:", suma
  !$omp parallel private(id_thread) reduction(+:suma)
  id_thread = omp_get_thread_num()
  
  !Se abre un bucle paralelo, donde los indices repetidos son privados por defecto, pero no los declaramos explicitamente (se encarga OpenMP). El número de iteraciones se reparte de la forma más equitativa posible entre los hilos.
 
    do i = 1,64
    	suma =suma +i
    	print*, "El índice es i=",i,"desde el hilo", id_thread
    end do

        print*, "El resultado de la suma en paralelo es:", suma, "desde el hilo", id_thread
 !Cerramos la región paralela 
 !$omp end parallel
     print*, "El resultado de la suma en paralelo es:", suma
 end program suma_sec


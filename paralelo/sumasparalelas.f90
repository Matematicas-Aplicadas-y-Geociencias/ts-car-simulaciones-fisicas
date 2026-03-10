program suma_openmp
  use omp_lib
  implicit none

  integer(8) :: N, inicio, fin, local, S, chunk, residuo, i
  integer :: id_thread, nthreads

  N = 10 
  S = 0

  !$omp parallel private(id_thread,nthreads,inicio,fin,local,chunk,residuo,i) shared(N,S)
    id_thread = omp_get_thread_num()
    nthreads  = omp_get_num_threads()

    chunk = N / nthreads   ! tamaño base de números que recibe cada thread
    residuo     = mod(N, nthreads) ! residuo de la división N/nthreads
! entonces qué pasa? cada thread recibe "chunk" números, pero sobran r números.
! esos r números se reparten dando un elemento extra a los primeros r threads.
! es decir:
! los primeros r threads reciben chunk+1 números
! los demás reciben chunk números

    write(*,*) "r=", residuo, " chunk=", chunk

    if (id_thread < residuo) then
            write(*,*) id_thread
      inicio = id_thread * (chunk + 1) + 1 ! inicio es "id_tread es menor a r, entonces toma el id_thread y multiplicalo por chunk+1
!      y sumale 1, así si id_thread es 0, entonces 0*2+1=1=inicio, si id_thread=1, netonces 1*2+1=3=inicio, etc. los finales salen
!      de inmediato
      fin = inicio + chunk 
    else!aquí tenems el primer caso, id_Thred=r, entonces inicio=(suponiendo N=20 y nthreads=12)8*2+0*1+1=17
            !si id_thread=9, 8*2+(9-8)*1+1=16+1+1=18
            !notar que solo suman chunk numeros!!!! no chuck + 1 
      inicio = residuo * (chunk + 1) + (id_thread - residuo) * chunk + 1
      fin = inicio + chunk -1
    end if

    write(*,*) "inicio= ", inicio, " fin= ", fin
    local = 0
    do i = inicio, fin
      local = local + i
    end do

    !$omp atomic
    S = S + local
  !$omp end parallel

  write(*,*) "N =", N, " S =", S, " (esperado =", N*(N+1)/2, ")"
end program suma_openmp

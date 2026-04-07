module utiles
  !
  implicit none
  !
  ! Iteradores y tamaño del problema
  !
  integer, parameter :: nx = 60, ny = 30, itermax=10000
  !
contains
  !
  Subroutine residuo_temp(temp,deltax,deltay,resid)
    !
    ! Esta subrutina eval'ua el laplaciano de un campo escalar y devuelve
    ! su elemento m'as grande
    !
    use omp_lib
    !
    implicit none
    !
    double precision, intent(in)  :: temp(:,:)
    double precision, intent(in)  :: deltax,deltay
    double precision, intent(out) :: resid
    !
    integer  ::  sx, sy
    integer  ::  ii, jj
    !
    ! ----------------------------------------------
    !
    ! Se determinan las dimensiones del arreglo temp
    !
    sx = size(temp,1)
    sy = size(temp,dim=2)
    !
    ! Se recorren los puntos de la malla
    !
    ! do jj = 2, sy-1
    !    !
    !    do ii = 2, sx-1
    !       !
    !       resid = ( temp(ii-1,jj,1)-2.d0*temp(ii,jj,1) + &
    !            & temp(ii+1,jj,1) )/(deltax*deltax)+&
    !            & ( temp(ii,jj-1,1)-2.d0*temp(ii,jj,1) +&
    !            temp(ii,jj+1,1) )/(deltay*deltay)
    !    end do
    !    !
    ! end do
    
  end Subroutine residuo_temp
  !
  !-----------------------------------------
  !
  ! Subrutina de postproceso de archivos vtk
  !
  subroutine postproceso_vtk(&
       &xo,yo,tempo,archivoo&
       &)
    ! use malla, only : mi, nj, DBL, mic, njc, zkc
    implicit none
    INTEGER :: i,j,k
    double precision, DIMENSION(nx),    INTENT(in)      :: xo
    double precision, DIMENSION(ny),    INTENT(in)      :: yo
    double precision, DIMENSION(nx,ny), INTENT(in)      :: tempo
    CHARACTER(48), INTENT(in)                         :: archivoo
    character(64)                                     :: mico,njco,zkco
    character(128)                                    :: npuntosc
    !
    ! Creaci\'on de cadenas de caracteres para el contenido de los archivos
    !
    write(mico,*) nx
    write(njco,*) ny
    write(zkco,*) 1
    write(npuntosc,*) (nx)*(ny)*1
    !
    !************************************
    ! VTK
    open(78, file = trim(archivoo), access='stream', convert="big_endian")

    write(78) '# vtk DataFile Version 2.3'//new_line(' ')
    write(78) '3D Mesh'//new_line(' ')
    write(78) 'BINARY'//new_line(' ')
    write(78) 'DATASET STRUCTURED_GRID'//new_line(' ')
    write(78) 'DIMENSIONS '//trim(mico)//trim(njco)//trim(zkco)//new_line('a')
    write(78) 'POINTS '//trim(npuntosc)//' float',new_line('a')
    do k = 1, 1
       do j = 1, ny
          do i = 1, nx
             write(78) real(xo(i)),real(yo(j)),0.0
          enddo
       enddo
    end do
    write(78) new_line('a')//'POINT_DATA '//trim(npuntosc)
    write(78) 'SCALARS TEMPER float',new_line('a')
    write(78) 'LOOKUP_TABLE default',new_line('a')
    do k = 1, 1
       do j =1, ny
          do i =1, nx
             write(78) real(tempo(i,j))
             ! write(78) real(bo(i,j))
          end do
       end do
    end do
    ! write(78) new_line('a')//'SCALARS TEMPER float',new_line('a')
    ! write(78) 'LOOKUP_TABLE default',new_line('a')
    ! do k = 1, 1
    !    do j =1, nj+1
    !       do i =1, mi+1
    !          write(78) real(tempo(i,j))
    !       end do
    !    end do
    ! end do
    ! write(78) new_line('a')//'VECTORS VELOCITY float',new_line('a')
    ! do k = 1, 1
    !    do j =1, nj+1
    !       do i =1, mi+1
    !          write(78) real(uo(i,j)),real(vo(i,j)),0.0 
    !       end do
    !    end do
    ! end do
    close(78)
    !
    ! 100 FORMAT(3(f12.6));
    ! 110 FORMAT(A);
    ! 111 FORMAT(A,/);
    ! 120 FORMAT(A,I4,I4,I4);
    ! 130 FORMAT(A,I10,A);
    ! 140 FORMAT(A,I10);
    !
  end subroutine postproceso_vtk
  
end module utiles

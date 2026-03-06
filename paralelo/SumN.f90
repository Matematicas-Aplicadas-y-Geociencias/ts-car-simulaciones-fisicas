program suma_parallel_regions
    use omp_lib
    implicit none
    integer :: suma_total
    
    suma_total = 0
    
    !$omp parallel reduction(+:suma_total)
    
    ! Cada hilo suma un número específico usando su thread_id
    if (omp_get_thread_num() == 0) suma_total = suma_total + 1
    if (omp_get_thread_num() == 1) suma_total = suma_total + 2
    if (omp_get_thread_num() == 2) suma_total = suma_total + 3
    if (omp_get_thread_num() == 3) suma_total = suma_total + 4
    if (omp_get_thread_num() == 4) suma_total = suma_total + 5
    if (omp_get_thread_num() == 5) suma_total = suma_total + 6
    if (omp_get_thread_num() == 6) suma_total = suma_total + 7
    if (omp_get_thread_num() == 7) suma_total = suma_total + 8
    if (omp_get_thread_num() == 8) suma_total = suma_total + 9
    if (omp_get_thread_num() == 9) suma_total = suma_total + 10
    if (omp_get_thread_num() == 10) suma_total = suma_total + 11
    if (omp_get_thread_num() == 11) suma_total = suma_total + 12
    if (omp_get_thread_num() == 12) suma_total = suma_total + 13
    if (omp_get_thread_num() == 13) suma_total = suma_total + 14
    if (omp_get_thread_num() == 14) suma_total = suma_total + 15
    if (omp_get_thread_num() == 15) suma_total = suma_total + 16
    if (omp_get_thread_num() == 16) suma_total = suma_total + 17
    if (omp_get_thread_num() == 17) suma_total = suma_total + 18
    if (omp_get_thread_num() == 18) suma_total = suma_total + 19
    if (omp_get_thread_num() == 19) suma_total = suma_total + 20
    if (omp_get_thread_num() == 20) suma_total = suma_total + 21
    if (omp_get_thread_num() == 21) suma_total = suma_total + 22
    if (omp_get_thread_num() == 22) suma_total = suma_total + 23
    if (omp_get_thread_num() == 23) suma_total = suma_total + 24
    if (omp_get_thread_num() == 24) suma_total = suma_total + 25
    if (omp_get_thread_num() == 25) suma_total = suma_total + 26
    if (omp_get_thread_num() == 26) suma_total = suma_total + 27
    if (omp_get_thread_num() == 27) suma_total = suma_total + 28
    if (omp_get_thread_num() == 28) suma_total = suma_total + 29
    if (omp_get_thread_num() == 29) suma_total = suma_total + 30
    if (omp_get_thread_num() == 30) suma_total = suma_total + 31
    if (omp_get_thread_num() == 31) suma_total = suma_total + 32
    if (omp_get_thread_num() == 32) suma_total = suma_total + 33
    if (omp_get_thread_num() == 33) suma_total = suma_total + 34
    if (omp_get_thread_num() == 34) suma_total = suma_total + 35
    if (omp_get_thread_num() == 35) suma_total = suma_total + 36
    if (omp_get_thread_num() == 36) suma_total = suma_total + 37
    if (omp_get_thread_num() == 37) suma_total = suma_total + 38
    if (omp_get_thread_num() == 38) suma_total = suma_total + 39
    if (omp_get_thread_num() == 39) suma_total = suma_total + 40
    if (omp_get_thread_num() == 40) suma_total = suma_total + 41
    if (omp_get_thread_num() == 41) suma_total = suma_total + 42
    if (omp_get_thread_num() == 42) suma_total = suma_total + 43
    if (omp_get_thread_num() == 43) suma_total = suma_total + 44
    if (omp_get_thread_num() == 44) suma_total = suma_total + 45
    if (omp_get_thread_num() == 45) suma_total = suma_total + 46
    if (omp_get_thread_num() == 46) suma_total = suma_total + 47
    if (omp_get_thread_num() == 47) suma_total = suma_total + 48
    if (omp_get_thread_num() == 48) suma_total = suma_total + 49
    if (omp_get_thread_num() == 49) suma_total = suma_total + 50
    if (omp_get_thread_num() == 50) suma_total = suma_total + 51
    if (omp_get_thread_num() == 51) suma_total = suma_total + 52
    if (omp_get_thread_num() == 52) suma_total = suma_total + 53
    if (omp_get_thread_num() == 53) suma_total = suma_total + 54
    if (omp_get_thread_num() == 54) suma_total = suma_total + 55
    if (omp_get_thread_num() == 55) suma_total = suma_total + 56
    if (omp_get_thread_num() == 56) suma_total = suma_total + 57
    if (omp_get_thread_num() == 57) suma_total = suma_total + 58
    if (omp_get_thread_num() == 58) suma_total = suma_total + 59
    if (omp_get_thread_num() == 59) suma_total = suma_total + 60
    if (omp_get_thread_num() == 60) suma_total = suma_total + 61
    if (omp_get_thread_num() == 61) suma_total = suma_total + 62
    if (omp_get_thread_num() == 62) suma_total = suma_total + 63
    if (omp_get_thread_num() == 63) suma_total = suma_total + 64
    if (omp_get_thread_num() == 64) suma_total = suma_total + 65
    if (omp_get_thread_num() == 65) suma_total = suma_total + 66
    if (omp_get_thread_num() == 66) suma_total = suma_total + 67
    if (omp_get_thread_num() == 67) suma_total = suma_total + 68
    if (omp_get_thread_num() == 68) suma_total = suma_total + 69
    if (omp_get_thread_num() == 69) suma_total = suma_total + 70
    if (omp_get_thread_num() == 70) suma_total = suma_total + 71
    if (omp_get_thread_num() == 71) suma_total = suma_total + 72
    if (omp_get_thread_num() == 72) suma_total = suma_total + 73
    if (omp_get_thread_num() == 73) suma_total = suma_total + 74
    if (omp_get_thread_num() == 74) suma_total = suma_total + 75
    if (omp_get_thread_num() == 75) suma_total = suma_total + 76
    if (omp_get_thread_num() == 76) suma_total = suma_total + 77
    if (omp_get_thread_num() == 77) suma_total = suma_total + 78
    if (omp_get_thread_num() == 78) suma_total = suma_total + 79
    if (omp_get_thread_num() == 79) suma_total = suma_total + 80
    if (omp_get_thread_num() == 80) suma_total = suma_total + 81
    if (omp_get_thread_num() == 81) suma_total = suma_total + 82
    if (omp_get_thread_num() == 82) suma_total = suma_total + 83
    if (omp_get_thread_num() == 83) suma_total = suma_total + 84
    if (omp_get_thread_num() == 84) suma_total = suma_total + 85
    if (omp_get_thread_num() == 85) suma_total = suma_total + 86
    if (omp_get_thread_num() == 86) suma_total = suma_total + 87
    if (omp_get_thread_num() == 87) suma_total = suma_total + 88
    if (omp_get_thread_num() == 88) suma_total = suma_total + 89
    if (omp_get_thread_num() == 89) suma_total = suma_total + 90
    if (omp_get_thread_num() == 90) suma_total = suma_total + 91
    if (omp_get_thread_num() == 91) suma_total = suma_total + 92
    if (omp_get_thread_num() == 92) suma_total = suma_total + 93
    if (omp_get_thread_num() == 93) suma_total = suma_total + 94
    if (omp_get_thread_num() == 94) suma_total = suma_total + 95
    if (omp_get_thread_num() == 95) suma_total = suma_total + 96
    if (omp_get_thread_num() == 96) suma_total = suma_total + 97
    if (omp_get_thread_num() == 97) suma_total = suma_total + 98
    if (omp_get_thread_num() == 98) suma_total = suma_total + 99
    if (omp_get_thread_num() == 99) suma_total = suma_total + 100
    
    !$omp end parallel
    
    write(*,*) 'La suma del 1 al 100 es:', suma_total
    
end program suma_parallel_regions

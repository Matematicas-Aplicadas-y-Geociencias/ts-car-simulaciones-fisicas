#include<iostream>
#include<cmath>
#include<vector>
#include<fstream>

using namespace std;


int main(int argc, char* argv[]){

    if(argc != 4){
        fprintf(stderr, "ERROR! Usage: %s <N> <T1> <T2>\n", argv[0]);
        return 1;
    }

    int N     = stoi(argv[1]);
    double T1 = stod(argv[2]);
    double T2 = stod(argv[3]);
    //Inicializar la matriz con todos ceros

    double L = 1.0; // Longitud del dominio
    double dx = L/(N-1); // Tamaño del paso

    vector<double> boundary_conditions(2, 0.0);
    vector<double> rr(N, -1.0);        
    //Dar valores al vector de frontera
    boundary_conditions.at(0)   = T1;
    boundary_conditions.at(1) = T2;

    vector<double> aa(N, 0);
    vector<double> bb(N, 0);
    vector<double> cc(N, 0);

    //Llenar la matriz con los valores correspondientes
    for(int i = 1; i < N-1; i++){
        aa[i] =  1.0;
        bb[i] = -2.0;
        cc[i] =  1.0;
        rr[i] =  0.0;  
    }

    //Condiciones de frontera x=0
    aa[0] = 0.0;
    bb[0] = 1.0;
    cc[0] = 0.0;
    rr[0] = boundary_conditions.at(0);
    //Condiciones de frontera x=L
    aa[N-1] = 0.0;
    bb[N-1] = 1.0;
    cc[N-1] = 0.0;
    rr[N-1] = boundary_conditions.at(1);

    //Resolver el sistema de ecuaciones utilizando el método de Thomas
    //Forward elimination, la dejamos en forma triangular superior
    for(int ii=1; ii < N; ii++){
        rr[ii] = rr[ii] - (aa[ii]/bb[ii-1])*rr[ii-1];
        bb[ii] = bb[ii] - (aa[ii]/bb[ii-1])*cc[ii-1];
    }
    //Resolvemos para el valor de la última fila
    rr[N-1] = rr[N-1]/bb[N-1];
    //Backward substitution, obtenemos el valor de las filas anteriores
    for(int jj=N-2; jj >= 0; jj--){
        rr[jj] = (rr[jj] - cc[jj]*rr[jj+1])/bb[jj];
    }

    //Imprimir los resultados
    for(int i = 0; i < N; i++){
        cout << i*dx << " " << rr[i] << endl;
    }

    return 0;


}
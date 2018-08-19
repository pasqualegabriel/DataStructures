#include <iostream>
#include "Fraccion.h"

using namespace std;

int main()
{
    Fraccion f1 = fraccion(5,4);
    cout << "Fraccion1 " << getX(f1) << "/" << getY(f1) << endl;
    Fraccion f2 = fraccion(5,4);
    cout << "Fraccion2 " << getX(f2) << "/" << getY(f2) << endl;
    multiplicar(f1,f2);
    cout << "f*  " << getX(f1) << "/" << getY(f1) << endl;
    cout << "f*  " << getX(f2) << "/" << getY(f2) << endl;
    destruir(f1);
    cout << endl << "f*  " << getX(f1) << "/" << getY(f1) << endl;
    return 0;
}



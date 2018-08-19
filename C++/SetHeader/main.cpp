#include <iostream>
#include "Set.h"

using namespace std;

int main()
{
    Set s = emptyS();
    cout << "Longitud = " << sizeS(s) << endl;
    cout << "Pertenece = " << belongs(1,s) << endl;
    mkAddS(4,s);
    mkAddS(3,s);
    mkAddS(2,s);
    mkAddS(1,s);
    mkAddS(1,s);
    imprimirS(s);
    cout << "Pertenece 1 = " << belongs(1,s) << endl;
    cout << "Pertenece 5 = " << belongs(5,s) << endl;
    return 0;
}


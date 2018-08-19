#include <iostream>
#include "Set.h"

using namespace std;

int main()
{
    Set s = singleton(4);
    s = addS(1,addS(2, addS(3, s)));
    Set d = emptyS();
    d = addS(1,addS(7, addS(5, d)));
    imprimirS(s);
    cout << endl << " 1 pertenece a s si = " << belongs(1,s) << endl;
    cout << "11 pertenece a s no = " << belongs(11,s) << endl;
    s = removeS(3, s);
    imprimirS(s);
    cout << endl;
    imprimirS(d);
    cout << endl;
    Set f = unionS(d,s);
    cout << endl << "union = ";
    imprimirS(f);
    return 0;
}


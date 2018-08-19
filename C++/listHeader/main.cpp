#include <iostream>
#include "usuarioListHeader.h"

using namespace std;

int main()
{
    List xs=nil();
    cout << "lista vacia 1 = " << isNil(xs) << endl;
    mkCons(1,xs);
    imprimir(xs);
    cout << "Size 1 = " << getSize(xs) << endl;
    mkSnoc(xs,2);
    mkSnoc(xs,3);
    cout << "Size 2 = " << getSize(xs) << endl;
    imprimir(xs);
    List ys=copiar(xs);
    imprimir(ys);
    cout << "Suma = " << sumar(xs) << endl;
    cout << "Length = " << length(xs) << endl;
    cout << "Succ a todos los elementos de la lista ";
    mapSucc(ys);
    imprimir(ys);
    List zs=nil();
    zs=takeN(2,ys);
    cout << "take 2 = ";
    imprimir(zs);
    cout << "pertenece 2 = " << pertenece(2,xs) << endl;
    cout << "pertenece 5 = " << pertenece(5,xs) << endl;
    imprimir(xs);
    imprimir(ys);
    unionL(xs,ys);
    cout << "Union = ";
    imprimir(xs);
    imprimir(ys);
    mkTail(ys);
    imprimir(ys);
    cout << "Head = " << ys << " = " << head(ys) << endl;
    cout << "Last = " << ys << " = " << last(ys) << endl;
    return 0;
}



#include <iostream>

using namespace std;

#include "pelicula.h"
#include "Heap.h"
#include "Map.h"

int main()
{
    ArrayList a = crearArrayListDe(4);
    add(a,"accion");
    add(a,"ficcion");
    Pelicula p1 = crearPelicula("Arrow",1,2,a);
    printP(p1);
    Pelicula p2 = crearPelicula("The 100",2,5,a);
    Pelicula p3 = crearPelicula("Gotham",3,6,a);
    Pelicula p4 = crearPelicula("Flash",4,3,a);
    Pelicula p5 = crearPelicula("Supergirl",5,7,a);
    Pelicula p6 = crearPelicula("Smallville",6,4,a);
    Pelicula p7 = crearPelicula("Legends",7,8,a);
    Pelicula p8 = crearPelicula("Dragon Ball",8,1,a);
    cout << endl;
    Heap h = emptyH(31);
    insertH(p1,h);
    insertH(p2,h);
    insertH(p3,h);
    insertH(p4,h);
    insertH(p5,h);
    insertH(p6,h);
    insertH(p7,h);
    insertH(p8,h);
    cout << "First element: " << titulo(findMin(h)) << endl << endl;
    printH(h);
    printL(h);
    cout << endl;

    Map m = emptyM();
    addM(m,generos(p1),titulo(p1));
    addM(m,generos(p2),titulo(p2));
    removeM(m,"accion");
    printMap(m);

    return 0;
}


#include <iostream>
#include "Fraccion.h"

struct FraccionSt
{
    int x;
    int y;
};

Fraccion fraccion(int x, int y)
{
    Fraccion f = new FraccionSt;
    f->x=x;
    f->y=y;
    return f;
}

getX(Fraccion f)
{
    return(f->x);
}

getY(Fraccion f)
{
    return(f->y);
}

Fraccion multiplicar(Fraccion f1, Fraccion f2)
{
    f1->x=f1->x*f2->x;
    f1->y=f1->y*f2->y;
    return f1;
}

void destruir(Fraccion& f1)
{
    delete f1;
    f1->x=NULL;
    f1->y=NULL;
}




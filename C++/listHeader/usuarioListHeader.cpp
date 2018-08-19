#include "usuarioListHeader.h"

int sumar(List xs)
{
    Iterador it = getIterador(xs);
    int res = 0;
    while(not atEnd(it))
    {
        res+=getElem(it);
        moveToNext(it);
    }
    res+=getElem(it);
    destroyIt(it);
    return res;
}

int length(List xs)
{
    Iterador it = getIterador(xs);
    int res = 0;
    while(not atEnd(it))
    {
        res++;
        moveToNext(it);
    }
    res++;
    destroyIt(it);
    return res;
}

void mapSucc(List& xs)
{
    Iterador it = getIterador(xs);
    while(not atEnd(it))
    {
        setElem(it,getElem(it)+1);
        moveToNext(it);
    }
    setElem(it,getElem(it)+1);
    destroyIt(it);
}

List takeN(int n, List xs)
{
    Iterador it = getIterador(xs);
    List res = nil();
    int nro = n;
    while(nro != 0)
    {
        mkSnoc(res,getElem(it));
        moveToNext(it);
        nro--;
    }
    destroyIt(it);
    return res;
}

bool pertenece(elem_type e,List xs)
{
    Iterador it = getIterador(xs);
    while(not atEnd(it))
    {
        if(e==getElem(it))
        {
            return true;
        }
        moveToNext(it);
    }
    destroyIt(it);
    return false;
}



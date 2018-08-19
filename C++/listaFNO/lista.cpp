#include "lista.h"

List nil()
{
    return (NULL);
}

bool isNil(List l)
{
    return(l==NULL);
}

List cons(elem_type e,List l)
{
    List xs = new LNode;
    xs->elem = e;
    xs->next = l;
    return xs;
}

elem_type head(List l)
{
    return (l->elem);
}

List tail(List l)
{
    return(l->next);
}

void imprimir(List l)
{
    cout << l << " = [";
    while(not (isNil(l)))
    {
        if(tail(l)==NULL)
        {
            cout << head(l);
            l=tail(l);
        }
        else
        {
            cout << head(l) << ",";
            l = tail(l);
        }
    }
    cout << "]";
}


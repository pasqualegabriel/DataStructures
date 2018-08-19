#include "usuarioLista.h"

int sumatoria(List l)
{
    int res = 0;
    while(not isNil(l))
    {
        res += head(l);
        l = tail(l);
    }
    return res;
}

int longitud(List l)
{
    int res = 0;
    while(not isNil(l))
    {
        res += 1;
        l = tail(l);
    }
    return res;
}

bool sonTodos(int e,List l)
{
    bool res = true;
    if(isNil(l))
    {
        return false;
    }
    while(not isNil(l))
    {
        res = res && (head(l)==e);
        l = tail(l);
    }
    return res;
}

bool sonTodosBis(int e,List l)
{
    if(isNil(l))
    {
        return(false);
    }
    while(not isNil(l)&&(head(l)==e))
    {
        l = tail(l);
    }
    return (isNil(l));
}

bool hayAlgun(int e,List l)
{
    bool res = false;
    while(not isNil(l))
    {
        res = res || (head(l)==e);
        l = tail(l);
    }
    return res;
}



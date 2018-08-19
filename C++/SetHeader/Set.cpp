#include "Set.h"

struct SetNode
{
    elem_type elem;
    SetNode* next;
};


struct SetHeader
{
    SetNode* first;
    SetNode* last;
    int len;
};

Set emptyS()
{
    Set newSet = new SetHeader;
    newSet->first=NULL;
    newSet->last=NULL;
    newSet->len=0;
    return newSet;
}

int sizeS(Set s)
{
    return s->len;
}

Iterador getIterador(Set l)//retorna un iterador que apunta al primero
{
     return l->first;
}


bool atEnd(Iterador it)//indica si el iterador llego al final
{
    return it->next==NULL;
}

elem_type getElem(Iterador it)//retorna el elemento en la posicion
{
    return it->elem;
}

void setElem(Iterador& it,elem_type e)//setea el elemento en la posiscion
{
    it->elem==e;
}

void moveToNext(Iterador& it)
{
    it=it->next;
}

void destroyIt(Iterador& it)
{
    it=NULL;
}

bool belongs(elem_type x, Set s)
{
    if(sizeS(s)==0) return false;
    Iterador i = getIterador(s);
    while(not atEnd(i))
    {
        if(getElem(i)==x)
        {
            return true;
        }
        moveToNext(i);
    }
    if(getElem(i)==x)
    {
        return true;
    }
    destroyIt(i);
    return false;
}

void mkAddS(elem_type e, Set& s)
{
    if(belongs(e,s)){}
    else{

    SetNode* newNode = new SetNode;
    newNode->elem = e;
    newNode->next=s->first;
    if(sizeS(s)==0)
    {
        s->last=newNode;
    }
    s->first=newNode;
    s->len++;

    }
}

void mkRemoveS(elem_type x, Set& s)
{

}

void mkUnionS(Set& s1, Set& s2)
{
    s1->len+=s2->len;
    s1->last->next=s2->first;
    s1->last=s2->last;
}

void imprimirS(Set s)
{
    Iterador i = getIterador(s);
    cout << s << " = [";
    while(not atEnd(i))
    {
        cout << getElem(i) << ",";
        moveToNext(i);
    }
    cout << getElem(i) << "]" << endl;
    destroyIt(i);
}


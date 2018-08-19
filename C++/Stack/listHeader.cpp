#include "listHeader.h"

struct LNode
{
    elem_type elem;
    LNode* next;
};

struct LHeader
{
    int sizeL;
    LNode* first;
    LNode* last;
};
//Inv Rep:

List nil()
{
    List newList = new LHeader;
    newList->sizeL=0;
    newList->first=NULL;
    newList->last=NULL;
    return newList;
}

bool isNil(List l)
{
    return l->sizeL==0;
}

void mkCons(elem_type e,List& l)
{
    LNode* newNode = new LNode;
    newNode->elem = e;
    newNode->next=l->first;
    if(isNil(l))
    {
        l->last=newNode;
    }
    l->first=newNode;
    l->sizeL++;
}

elem_type getSize(List l)
{
    return(l->sizeL);
}

void mkSnoc(List& l, elem_type e)
{
    LNode* newNode = new LNode;
    newNode->elem = e;
    newNode->next = NULL;
    if(isNil(l))
    {
        l->first=newNode;
    }
    else
    {
        l->last->next=newNode;
    }
    l->last=newNode;
    l->sizeL++;
}

Iterador getIterador(List l)
{
    return l->first;
}

bool atEnd(Iterador it)
{
    return it->next==NULL;
}

elem_type getElem(Iterador it)
{
    return it->elem;
}

void setElem(Iterador& it,elem_type e)
{
    it->elem=e;
}

//parcial en lista null
void moveToNext(Iterador& it)
{
    it=it->next;
}

void destroyIt(Iterador& it)
{
    it=NULL;
}

void imprimir(List l)
{
    if(l==NULL){cout << "Null" << endl;}
    else
    {
        Iterador it = getIterador(l);
        cout << l << " = [";
        while(not atEnd(it))
        {
            cout << getElem(it) << ",";
            moveToNext(it);
        }
        cout << getElem(it) << "]" << endl;
        destroyIt(it);
    }
}

List copiar(List l)
{
    List newList = new LHeader;
    newList = nil();
    Iterador it = getIterador(l);
    while(not atEnd(it))
    {
        mkSnoc(newList,getElem(it));
        moveToNext(it);
    }
    mkSnoc(newList,getElem(it));
    destroyIt(it);
    return newList;
}

void unionL(List& xs,List& ys)
{
    xs->sizeL+=ys->sizeL;
    xs->last->next=ys->first;
    xs->last=ys->last;
}

void mkTail(List& xs)
{
    LNode* temp = xs->first;
    xs->sizeL--;
    xs->first=xs->first->next;
    if(isNil(xs))
    {
        xs->last=NULL;
    }
    delete(temp);
    temp=NULL;
}

elem_type head(List xs)
{
    return xs->first->elem;
}

elem_type last(List xs)
{
    return xs->last->elem;
}






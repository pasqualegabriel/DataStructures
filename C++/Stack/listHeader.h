#include <iostream>

using namespace std;

typedef int elem_type;

struct LNode;

struct LHeader;

typedef LHeader* List;

typedef LNode* Iterador;

List nil();
bool isNil(List l);
void mkCons(elem_type e,List& l);
elem_type getSize(List l);
void mkSnoc(List& l, elem_type e);

Iterador getIterador(List l);//retorna un iterador que apunta al primero
bool atEnd(Iterador it);//indica si el iterador llego al final
elem_type getElem(Iterador it);//retorna el elemento en la posicion
void setElem(Iterador& it,elem_type e);//setea el elemento en la posiscion
void moveToNext(Iterador& it);
void destroyIt(Iterador& it);

void imprimir(List l);
List copiar(List l);

void unionL(List& xs,List& ys);
void mkTail(List& xs);
elem_type head(List xs);
elem_type last(List xs);


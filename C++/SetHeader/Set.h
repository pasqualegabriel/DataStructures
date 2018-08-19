#include <iostream>

using namespace std;

typedef int elem_type;

struct SetNode;

struct SetHeader;

typedef SetHeader* Set;

typedef SetNode* Iterador;

Set emptyS();
bool belongs(elem_type x, Set s);
void mkAddS(elem_type x, Set& s);
int sizeS(Set s);
void mkRemoveS(elem_type x, Set& s);
void mkUnionS(Set& s1, Set& s2);
void imprimirS(Set s);
void destroyS(Set& s);

Iterador getIterador(Set l);//retorna un iterador que apunta al primero
bool atEnd(Iterador it);//indica si el iterador llego al final
elem_type getElem(Iterador it);//retorna el elemento en la posicion
void setElem(Iterador& it,elem_type e);//setea el elemento en la posiscion
void moveToNext(Iterador& it);
void destroyIt(Iterador& it);

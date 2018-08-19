#include <iostream>

using namespace std;

typedef int elem_type;

struct LNode
{
    elem_type elem;
    LNode* next;
};

typedef LNode* List;

List nil();
bool isNil(List l);
elem_type head(List l);
List tail(List l);
List cons(elem_type elem,List l);
void imprimir(List l);


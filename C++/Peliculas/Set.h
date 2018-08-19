#include <iostream>

using namespace std;

typedef int elem_type;

struct SetNode {
  elem_type x;
  SetNode* next;
};

typedef SetNode* Set;

Set emptyS();
Set singleton(elem_type e);
bool belongs(elem_type x, Set s);
Set addS(elem_type x, Set s1);
Set removeS(elem_type x, Set s1);
Set unionS(Set s1, Set s2);
void imprimirS(Set s);
void destroyS(Set& s);

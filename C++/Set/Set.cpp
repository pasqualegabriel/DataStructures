#include "Set.h"

Set emptyS()
{
	 return NULL;
}

Set singleton(elem_type e)
{
  Set node = new SetNode;
  node->x = e;
  node->next = NULL;
  return node;
}

bool belongs(elem_type x, Set s) {
  while(s != NULL) {
    if(s->x == x) { return true; }
    s = s->next;
  }
  return false;
}

Set addS(elem_type x , Set s)
{
  Set copia = s;
  while(copia!=NULL)
  {
  	if(copia->x==x)
    {
        return s;
    }
    copia=copia->next;
  }
  Set ss = new SetNode;
  ss->x = x;
  ss->next = s;
  return ss;
}

Set removeS(elem_type x, Set s) {
  // if(s == NULL) return s;
  // Solo para no crear las variables

  Set principio  = s;
  Set anterior   = NULL;
  Set actual     = s;

  while(actual != NULL && actual->x != x) {
    anterior = actual;
    actual   = actual->next;
  }

  if(actual != NULL && actual->x == x) {
  	Set aEliminar = actual;
  	if(anterior == NULL) {
  		principio = actual->next;
  	} else {
	  	anterior->next = actual->next;
  	}
  	delete aEliminar;
  }

  return principio;
}

Set unionS(Set s1, Set s2) {
    Set ss = new SetNode;
	while(s2 != NULL) {
		addS(s2->x, ss);
		s2=s2->next;
	}
		while(s1 != NULL) {
		addS(s1->x, ss);
		s1=s1->next;
	}
	return ss;
}

void destroyS(Set& s) {
	while(s != NULL) {
		Set tmp = s;
		s = s->next;
		delete tmp;
	}
}

void imprimirS(Set s)
{
    cout << s << " = [";
    while(s != NULL)
    {
        if(s->next==NULL)
        {
            cout << s->x;
            s=s->next;
        }
        else
        {
            cout << s->x << ",";
            s=s->next;
        }
    }
    cout << "]";
}


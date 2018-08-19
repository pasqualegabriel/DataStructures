#include <iostream>

using namespace std;

#define elem_type int

struct HNode;

typedef HNode* Heap;


Heap emptyH(int m);

bool isEmptyH(Heap h);

void insertH(elem_type x,Heap& h);

elem_type findMin(Heap h);

void deleteMin(Heap& h);

elem_type splitMin(Heap& h);

elem_type splitMinSimple(Heap& h);

void destroyH(Heap& h);

void printH(Heap h);

void printL(Heap h);



#include <iostream>

using namespace std;

struct SNode;

typedef SNode* Stack;

Stack emptyS();
bool isEmptyS(Stack s);
Stack push(int x, Stack s);
int top(Stack s);
Stack pop(Stack s);
int sizeS(Stack s);
void imprimirS(Stack s);
void destroyS(Stack& s);




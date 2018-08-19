#include "stack.h"

struct SNode
{
    int elem;
    SNode* next;
};

typedef SNode* Stack;

Stack emptyS()
{
    return NULL;
}

bool isEmptyS(Stack s)
{
    return s==NULL;
}

Stack push(int x, Stack s)
{
        Stack newStack = new SNode;
        newStack->elem=x;
        newStack->next=s;
        return newStack;
}

int sizeS(Stack s)
{
    int res=0;
    while(!isEmptyS(s))
    {
        res++;
        s=s->next;
    }
    return res;
}

int top(Stack s)
{
    return s->elem;
}

Stack pop(Stack s)
{
    return s->next;
}

void imprimirS(Stack s)
{
    cout << s << " = [";
    while(s->next!=NULL)
    {
        cout << s->elem << ",";
        s=s->next;
    }
    cout << "]" << endl;
}
/*
void destroyS(Stack& s)
{

}*/






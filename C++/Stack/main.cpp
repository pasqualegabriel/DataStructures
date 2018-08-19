#include <iostream>
#include "stack.h"
using namespace std;

int main()
{
    Stack s = emptyS();
    cout << "isEmpty = " << (isEmptyS(s) ? "SI" : "NO") << endl;
    s=push(2,s);
    s=push(1,s);
    cout << "Size = " << sizeS(s) << endl;
    cout << "isEmpty = " << (isEmptyS(s) ? "SI" : "NO") << endl;
    imprimirS(s);
    s=pop(s);
    cout << "Size = " << sizeS(s) << endl;
    imprimirS(s);
    return 0;
}



#include <iostream>
#include "Heap.h"
using namespace std;

int main()
{
    Heap h = emptyH(31);
    insertH(5,h);
    insertH(2,h);
    insertH(3,h);
    insertH(4,h);
    insertH(1,h);
    insertH(6,h);
    insertH(7,h);
    insertH(8,h);
    insertH(9,h);
    insertH(15,h);
    insertH(11,h);
    insertH(12,h);
    insertH(13,h);
    insertH(17,h);
    insertH(10,h);
    insertH(16,h);
    insertH(14,h);
    insertH(18,h);
    insertH(19,h);
    insertH(22,h);
    insertH(21,h);
    insertH(20,h);
    insertH(23,h);
    cout << "First element: " << findMin(h) << endl << endl;
    printH(h);
    deleteMin(h);
    printH(h);
    return 0;
}


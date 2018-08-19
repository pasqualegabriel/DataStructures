#include "Heap.h"

struct HNode
{
    int maxSize;
    int last;
    int* elems;
};

Heap emptyH(int m)
{
    HNode* newH = new HNode;
    newH->maxSize = m;
    newH->last = 0;
    newH->elems = new elem_type[m+1];
    newH->elems[0] = 0;
    return newH;
}

bool isEmptyH(Heap h)
{
    return h->last==0;
}

bool isFull(Heap h)
{
    return (h->maxSize==h->last);
}

void insertH(elem_type x,Heap& h)
{
    if(isFull(h)) return;
    int i=++h->last;
    // percolateUp
    while(h->elems[i/2]>x)
    {
        h->elems[i] = h->elems[i/2];
        i /= 2;
    }
    h->elems[i] = x;
}

elem_type findMin(Heap h)
{
    return h->elems[1];
}

void deleteMin(Heap& h)
{
    if(isEmptyH(h)) return;
    elem_type percolate = h->elems[h->last];
    h->last--;
    elem_type child = 2;
    //percolateDown
    for(elem_type i=1; (i*2) <= h->last; i=child)
    {
        child = i*2;
        //compiten hijos
        if(child != h->last && h->elems[child+1] < h->elems[child])
        {
            child++;
        }
        if(percolate>h->elems[child])
        {
            h->elems[i] = h->elems[child];
        }
        else break;
    }
    h->elems[child] = percolate;
}

elem_type splitMinSimple(Heap& h)
{
    elem_type res = findMin(h);
    deleteMin(h);
    return res;
}

void destroyH(Heap& h)
{
    delete[] h->elems;
    delete h;
}

void printH(Heap h)
{
    int i = 1;
    cout << "maxSize: " << h->maxSize << ", ";
    cout << "last: " << h->last << ",";
    cout << "[" << endl;
    cout << "                        " << h->elems[i];
    i++;
    while(i <= h->last)
    {
        if (i==2)
        {
            cout << endl << "            -------------------------";
            cout << endl << "            " << h->elems[i];
        } else if(i<4) {
           cout << "                       " << h->elems[i] << " ";
        } else
        if (i==4)
        {
            cout << endl << "      -------------           -------------";
            cout << endl << "      " << h->elems[i];
        } else if(i<8) {
           cout << "           " << h->elems[i] ;
        } else
        if (i==8)
        {
            cout << endl << "    ------    --------    --------    --------";
            cout << endl << "    " << h->elems[i];
        }
        else if(i<16)
        {
            cout << "    " << h->elems[i];
        } else if(i==16)
            {
            cout << endl << " ----- ----- ----- ----- ----- ----- ----- -----";
            cout << endl << " " << h->elems[i];
            } else{
            cout << " " << h->elems[i];
            }
        i++;
    }
    cout << endl << "]" << endl;
}

void printL(Heap h)
{
    int i = 1;
    cout << "maxSize: " << h->maxSize << ", ";
    cout << "last: " << h->last << ", ";
    cout << "[" << endl;
    while(i <= h->last)
    {
        cout << "  " << i << ": " << h->elems[i] << endl;
        i++;
    }
    cout << "]" << endl;
}



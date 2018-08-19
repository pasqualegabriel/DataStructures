#include "Heap.h"

struct HNode
{
    int maxSize;
    int last;
    elem_type* elems;
};

Heap emptyH(int m)
{
    HNode* newH = new HNode;
    newH->maxSize = m;
    newH->last = 0;
    newH->elems = new elem_type[m+1];
    Pelicula peli = crearPelicula("0",0,0,crearArrayList());
    newH->elems[0] = peli;
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
    while(puntaje(h->elems[i/2])>puntaje(x))
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
    int child = 2;
    //percolateDown
    for(int i=1; (i*2) <= h->last; i=child)
    {
        child = i*2;
        //compiten hijos
        if(child != h->last && puntaje(h->elems[child+1]) < puntaje(h->elems[child]))
        {
            child++;
        }
        if(puntaje(percolate)>puntaje(h->elems[child]))
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
    h = NULL;
}

void printH(Heap h)
{
    int i = 1;
    cout << "maxSize: " << h->maxSize << ", ";
    cout << "last: " << h->last << ",";
    cout << "[" << endl;
    cout << "                        " << codigo(h->elems[i]);
    i++;
    while(i <= h->last)
    {
        if (i==2)
        {
            cout << endl << "            -------------------------";
            cout << endl << "            " << codigo(h->elems[i]);
        } else if(i<4) {
           cout << "                       " << codigo(h->elems[i]) << " ";
        } else
        if (i==4)
        {
            cout << endl << "      -------------           -------------";
            cout << endl << "      " << codigo(h->elems[i]);
        } else if(i<8) {
           cout << "           " << codigo(h->elems[i]) ;
        } else
        if (i==8)
        {
            cout << endl << "    ------    --------    --------    --------";
            cout << endl << "    " << codigo(h->elems[i]);
        }
        else if(i<16)
        {
            cout << "    " << codigo(h->elems[i]);
        } else if(i==16)
            {
            cout << endl << " ----- ----- ----- ----- ----- ----- ----- -----";
            cout << endl << " " << codigo(h->elems[i]);
            } else{
            cout << " " << codigo(h->elems[i]);
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
        cout << "  " << i << ": " << titulo(h->elems[i]) << endl;
        i++;
    }
    cout << "]" << endl;
}



#include "pelicula.h"

struct peliSt
{
    ArrayList generos;
    string titulo;
    int codigo;
    int puntaje;
};

Pelicula crearPelicula(string t, int c, int nro, ArrayList g)
{
    Pelicula p = new peliSt;
    p->puntaje = nro;
    p->generos = g;
    p->titulo = t;
    p->codigo = c;
    return p;
}

int codigo(Pelicula p)
{
    return p->codigo;
}

int puntaje(Pelicula p)
{
    return p->puntaje;
}

string titulo(Pelicula p)
{
    return p->titulo;
}

ArrayList generos(Pelicula p)
{
    return p->generos;
}

bool igualA(string c, Pelicula p)
{
    return p->titulo==c;
}

void destroyPelicula(Pelicula& p)
{
    destroyArrayList(p->generos);
    delete p;
    p = NULL;
}

void printP(Pelicula p)
{
    cout << titulo(p) << endl;
    cout << "Codigo: " << codigo(p) << endl;
    cout << "Puntaje: " << puntaje(p) << endl;
    cout << "Generos: ";
    imprimirArrayList(p->generos);
}


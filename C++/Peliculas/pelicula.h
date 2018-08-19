#include <iostream>

#include "ArrayList.h"

using namespace std;

struct peliSt;

typedef peliSt* Pelicula;

Pelicula crearPelicula(string t, int c, int nro, ArrayList g);
int puntaje(Pelicula p);
int codigo(Pelicula p);
string titulo(Pelicula p);
ArrayList generos(Pelicula p);
bool igualA(string c, Pelicula p);
void destroyPelicula(Pelicula& p);
void printP(Pelicula p);



#include <iostream>
#include <stdlib.h>
#include "pelicula.h"
#include "ArrayList.h"

using namespace std;

struct gpSt
{
    string genero;
    ArrayList peliculas;
};

typedef gpSt* GeneroPelicula;

struct AVLNode {
    int height;
    AVLNode* left;
    AVLNode* right;
    GeneroPelicula kv;
};

typedef AVLNode* AVL;

typedef AVL Map;

// Prop.: crea un Map vacio
Map emptyM();

// Prop.: agrega un GeneroPelicula
void addM(Map& m, ArrayList g, string nombre);

// Prop.: devuelve la pelicula
GeneroPelicula lookupM(Map& m, string genero);

// Prop.: elimina una pelicula
void removeM(Map& m, string titulo);

// Prop.: devuelve la lista de peliculas de un Map
ArrayList domM(Map& m);

// Prop.: imprime un Map
void printMap(Map m);

// Prop.: libera la memoria de un Map
void destroyM(Map& m);

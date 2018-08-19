#include <iostream>

using namespace std;

struct FraccionSt;

typedef FraccionSt* Fraccion;

Fraccion fraccion(int x, int y);

int getX(Fraccion f);

int getY(Fraccion f);

Fraccion sumar(Fraccion f1, Fraccion f2);

Fraccion restar(Fraccion f1, Fraccion f2);

Fraccion multiplicar(Fraccion f1, Fraccion f2);

Fraccion dividir(Fraccion f1, Fraccion f2);

void invertir(Fraccion& f1);

void sumplificar(Fraccion& f1);

void destruir(Fraccion& f1);



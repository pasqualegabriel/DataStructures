#include <iostream>

using namespace std;

struct PersonaSt{
    string nombre;
    int energia;
};

typedef PersonaSt* Persona;

Persona crearPersona (string nombre, int energia);

int getEnergia(Persona& p);

string getNombre(Persona& p);

//pierde un punto de energia
void perderEnergia (Persona& p);

//muere Persona
void muerePersona(Persona& p);



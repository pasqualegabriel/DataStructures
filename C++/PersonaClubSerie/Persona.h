#include <iostream>
#include "Serie.h"
#include "Club.h"

using namespace std;

struct PersonaSt
{
    string nombre;
    int edad;
    Serie seriepelicula;
    Club equipo;
};

typedef PersonaSt* Persona;

Persona crearPersona(string nombre,int edad);

string getNombreP(Persona p);
int getEdad(Persona p);

void addClub(Persona& p,Club c);
void addSerie(Persona& p,Serie s);
void cumpleAnios(Persona& p);


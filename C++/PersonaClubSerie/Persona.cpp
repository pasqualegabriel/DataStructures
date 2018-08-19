#include "Persona.h"
#include "Serie.h"
#include "Club.h"

Persona crearPersona(string n,int e)
{
    Persona p = new PersonaSt;
    p->nombre = n;
    p->edad = e;
    p->seriepelicula = NULL;
    p->equipo = NULL;
    return (p);
}

string getNombreP(Persona p)
{
    return(p->nombre);
}

int getEdad(Persona p)
{
    return(p->edad);
}

void addSerie(Persona& p,Serie s)
{
    p->seriepelicula = s;
}

void addClub(Persona& p,Club c)
{
    p->equipo = c;
}

void cumpleAnios(Persona& p)
{
    p->edad++;
}



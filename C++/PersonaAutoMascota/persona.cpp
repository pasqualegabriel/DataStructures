#include "persona.h"

Persona crearPersona (string n, int e)
{
    Persona  p = new PersonaSt;
    p -> energia = e;
    p -> nombre = n;
    return (p);
}

//pierde un punto de energia
void perderEnergia (Persona& p)
{
    p -> energia --;
}

//muere Persona
void muerePersona(Persona& p)
{
    delete p;
    p = NULL;
}

int getEnergia(Persona& p)
{
    return (p -> energia);
}

string getNombre(Persona& p)
{
    return (p -> nombre);
}


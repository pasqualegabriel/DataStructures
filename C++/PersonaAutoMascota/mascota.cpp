#include "mascota.h"

struct MascotaSt{
    string nombre;
    int edad;
};

Mascota crearMascota(string n, int e)
{
    Mascota m = new MascotaSt;
    m -> edad = e;
    m -> nombre = n;
    return (m);
}

//pierde un anio de edad
void perderEnergia (Mascota& m)
{
    m -> edad --;
}

//muere Mascota
void muereMascota(Mascota& m)
{
    delete m;
    m = NULL;
}

int getEdadM(Mascota& m)
{
    return (m -> edad);
}

string getNombreM(Mascota& m)
{
    return (m -> nombre);
}

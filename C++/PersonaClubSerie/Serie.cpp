#include "Serie.h"

struct SerieSt
{
    string nombre;
    int capitulos;
};

Serie crearSerie(string n,int c)
{
    Serie s = new SerieSt;
    s->nombre = n;
    s->capitulos = c;
    return s;
}

string getNombreSerie(Serie s)
{
    return (s->nombre);
}

int getCapitulos(Serie s)
{
    return (s->capitulos);
}

void agregarCapitulo(Serie& s)
{
    s->capitulos++;
}











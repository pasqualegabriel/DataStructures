#include "Club.h"

struct ClubSt
{
    string nombre;
    int partidos;
};

string getNombreClub(Club c)
{
    return (c->nombre);
}

int getPartidos(Club c)
{
    return(c->partidos);
}

Club crearClub(string nombre,int partidos)
{
    Club c = new ClubSt;
    c->nombre = nombre;
    c->partidos = partidos;
    return c;
}

void jugarPartido(Club c)
{
    c->partidos++;
}


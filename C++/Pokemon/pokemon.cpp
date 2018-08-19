#include "Pokemon.h"

Pokemon crearPokemon(int vida, string nombre)
{
    Pokemon p = new PokemonSt;
    p -> vida = vida;
    p -> nombre = nombre;
    return p;
}

void restarVida(Pokemon& p)
{
    p -> vida-- ;
}

void cambiarNombre(Pokemon& p, string nombre)
{
    p -> nombre = nombre;
}

bool estaVivo(Pokemon p)
{
    return(p->vida>0);
}

string getNombre(Pokemon p)
{
    return (p->nombre);
}

int getVida(Pokemon p)
{
    return(p->vida);
}

void luchar(Pokemon& p, Pokemon& r)
{
    r->vida--;
    p->vida--;
}

void lucharN(int n, Pokemon& p, Pokemon& r)
{
    r->vida=r->vida-n;
    p->vida=p->vida-n;
}

void destruir(Pokemon& p)
{
    delete p;
    p = NULL;
}






#include <iostream>

using namespace std;

struct PokemonSt
{
    int vida;
    string nombre;
};

typedef PokemonSt* Pokemon;

Pokemon crearPokemon(int vida, string nombre);
void restarVida(Pokemon& p);
void cambiarNombre(Pokemon& p, string nombre);
bool estaVivo(Pokemon p);
string getNombre(Pokemon p);
int getVida(Pokemon p);
void luchar(Pokemon& p, Pokemon& r);
void lucharN(int n, Pokemon& p, Pokemon& r);
void destruir(Pokemon& p);

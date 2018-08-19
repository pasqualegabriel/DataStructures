#include <iostream>
#include "Pokemon.h"

using namespace std;

int main()
{
    Pokemon p = crearPokemon(80, "Pikachu");
    cout << "El nombre del pokemon es " << getNombre(p) << endl;
    cout << "La vida del pokemon es = " << getVida(p) << endl;
    restarVida(p);
    cout << "La vida del pokemon menos 1 es = " << getVida(p) << endl;
    cambiarNombre(p, "Raichu");
    cout << "El nombre del pokemon es " << getNombre(p) << endl;
    cout << "Si esta vivo 1, sino 0 " << endl << estaVivo(p) << endl;
    Pokemon r = crearPokemon(40, "Squirtle");
    cout << "El nombre de otro pokemon es " << getNombre(r) << endl;
    cout << "La vida del otro pokemon es = " << getVida(r) << endl;
    luchar(p,r);
    cout << "La vida del pokemon es = " << getVida(p) << endl;
    cout << "La vida del otro pokemon es = " << getVida(r) << endl;
    cout << "Luchan con 5 de ataque" << endl;
    lucharN(5,p,r);
    cout << "La vida del pokemon es = " << getVida(p) << endl;
    cout << "La vida del otro pokemon es = " << getVida(r) << endl;
    destruir(p);
    return 0;
}

#include <iostream>
#include "persona.h"
#include "auto.h"
#include "mascota.h"

using namespace std;

int main()
{
    Persona p = crearPersona("Ana", 100 );
    Persona p2 = crearPersona("carlos", 100 );
    Mascota m = crearMascota("Shelby", 20);
    cout << "Energia inicial = " << getEnergia(p) << endl;
    cout << "El nombre de la persona es " << getNombre(p) << endl;
    Auto a = crearAutoVacio("auto");
    subirPiloto(a ,p);
    subirCopiloto(a ,m);
    cout << "El piloto del auto es " << getNombre(a->piloto)  << endl;
    perderEnergia(p);
    perderEnergia(p);
    perderEnergia(p);
    perderEnergia(p);
    cout << "Energia al perder cuatro puntos de vida = " << getEnergia(p) << endl;
    cout << "Energia perdida = 4 " << endl;
    cout << "Mascota " << getNombreM(m) << " Edad " << getEdadM(m) << endl;
    return 0;
}

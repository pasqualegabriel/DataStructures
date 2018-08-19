#include <iostream>
#include "persona.h"
#include "auto.h"

using namespace std;

int main()
{
    Persona p = crearPersona("juan", 100 );
    Persona p2 = crearPersona("carlos", 100 );
    cout << "Energia inicial = " << getEnergia(p) << endl;
    cout << "El nombre de la persona es " << getNombre(p) << endl;
    Auto a = crearAutoVacio("auto");
    subirPiloto(a ,p);
    subirCopiloto(a ,p2);
    cout << "El piloto del auto es " << getNombre(a->piloto)  << endl;
    perderEnergia(p);
    perderEnergia(p);
    perderEnergia(p);
    perderEnergia(p);
    cout << "Energia al perder cuatro puntos de vida = " << getEnergia(p) << endl;
    cout << "Energia perdida = 4 " << endl;
    muerePersona(p);
    return 0;
}

#include <iostream>
#include "mascota.h"

using namespace std;

struct AutoSt{
    string marca;
    Persona piloto;
    Mascota copiloto;
};

typedef AutoSt* Auto;

Auto crearAutoVacio(string marca);

void subirPiloto(Auto& a, Persona p);

void subirCopiloto(Auto& a, Mascota m);

void bajaPiloto(Auto& a);

void bajarCopiloto(Auto& a);

void choqueYTodoExplota(Auto& a);

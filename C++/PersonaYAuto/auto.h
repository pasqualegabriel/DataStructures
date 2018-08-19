#include <iostream>

using namespace std;

struct AutoSt{
    string marca;
    Persona piloto;
    Persona copiloto;
};

typedef AutoSt* Auto;

Auto crearAutoVacio(string marca);

void subirPiloto(Auto& a, Persona p);

void subirCopiloto(Auto& a, Persona p);

void bajaPiloto(Auto& a);

void bajarCopiloto(Auto& a);

void choqueYTodoExplota(Auto& a);

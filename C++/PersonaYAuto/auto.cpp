#include "persona.h"
#include "auto.h"

Auto crearAutoVacio(string m){
    Auto  a = new AutoSt;
    a -> marca = m;
    a -> piloto = NULL;
    a -> copiloto = NULL;
    return (a);
}


void subirPiloto(Auto& a, Persona p){

    a -> piloto = p;

}

void subirCopiloto(Auto& a, Persona p)
{
    a -> copiloto = p;
}

void bajaPiloto(Auto& a)
{
    a -> piloto = NULL;
}

void bajarCopiloto(Auto& a)
{
        a -> copiloto = NULL;
}

void choqueYTodoExplota(Auto& a)
{
        muerePersona (a->piloto);
        muerePersona (a->copiloto);
        delete a;
        a = NULL;
}

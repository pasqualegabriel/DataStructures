#include "persona.h"
#include "mascota.h"
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

void subirCopiloto(Auto& a, Mascota p)
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
        muereMascota (a->copiloto);
        delete a;
        a = NULL;
}



#include <iostream>
#include "Serie.h"
#include "Club.h"
#include "Persona.h"

using namespace std;

int main()
{
    Serie s = crearSerie("Arrow",44);
    cout << "Serie " << getNombreSerie(s) << endl << "Capitulos " << getCapitulos(s) << endl;
    agregarCapitulo(s);
    cout << "Serie " << getNombreSerie(s) << endl << "Capitulos " << getCapitulos(s) << endl;
    Club c;
    c = crearClub("Boca",400);
    cout << "Club " << getNombreClub(c) << endl << "Partidos " << getPartidos(c) << endl;
    Persona p;
    p = crearPersona("Goku",24);
    addClub(p,c);
    addSerie(p,s);
    cout << "Persona " << getNombreP(p) << endl << "Edad " << getEdad(p) << endl;
    cout << "Serie de la Persona " << getNombreSerie(p->seriepelicula) << endl;
    return 0;
}


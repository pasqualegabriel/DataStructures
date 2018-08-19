#include <iostream>

using namespace std;

struct MascotaSt;

typedef MascotaSt* Mascota;

Mascota crearMascota(string nombre, int edad);

int getEdadM(Mascota& m);

string getNombreM(Mascota& m);

//pierde un anio de edad
void perderEdad(Mascota& m);

//muere Mascota
void muereMascota(Mascota& m);

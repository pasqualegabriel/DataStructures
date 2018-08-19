#include <iostream>

using namespace std;

struct SerieSt;

typedef SerieSt* Serie;

Serie crearSerie(string n,int c);

string getNombreSerie(Serie s);
int getCapitulos(Serie s);

void agregarCapitulo(Serie& s);

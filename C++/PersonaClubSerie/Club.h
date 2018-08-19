#include <iostream>

using namespace std;

struct ClubSt;

typedef ClubSt* Club;

string getNombreClub(Club c);
int getPartidos(Club c);

Club crearClub(string nombre,int partidos);
void jugarPartido(Club c);


#include <iostream>
#include "usuarioLista.h"

using namespace std;

int main()
{
    List l = nil();
    l = cons(1,cons(2,cons(3,cons(4,l))));
    List ls = nil();
    ls = cons(1,cons(1,cons(1,cons(1,ls))));
    imprimir(l);
    cout << endl << "Sumatoria = " << sumatoria(l);
    cout << endl << "Longitud = " << longitud(l);
    cout << endl << "sonTodos = " << sonTodos(1,l);
    cout << endl << "sonTodos = " << sonTodos(1,ls);
    cout << endl << "sonTodos = " << sonTodosBis(1,l);
    cout << endl << "sonTodos = " << sonTodosBis(1,ls);
    cout << endl << "hayAlgun = " << hayAlgun(1,l);
    cout << endl << "hayAlgun = " << hayAlgun(2,ls);
    return 0;
}


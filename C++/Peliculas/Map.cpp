#include "Map.h"

///////////////////////////////////////////////
/// OPERACIONES SOBRE AVL
///////////////////////////////////////////////

// Prop.: crea una hoja con determinado cliente
AVL leafAVL(GeneroPelicula c) {
    AVLNode* node = new AVLNode;
    node->height = 1;
    node->left   = NULL;
    node->right  = NULL;
    node->kv     = c;
    return node;
}

// Prop.: devuelve la altura de un AVL
int heightAVL(AVL t) {
    return t == NULL ? 0 : t->height;
}

// Prop.: hace una rotación simple
AVL sJoinAVL(GeneroPelicula kv, AVL ti, AVL td) {
    AVL node      = leafAVL(kv);
    node->height  = 1 + max(heightAVL(ti), heightAVL(td));
    node->left    = ti;
    node->right   = td;
    return node;
}

/// PRECOND: ti es dos más profundo que td (y por lo tanto es no vacio)
// Prop.: realiza una rotación a izquierda
AVL lJoinAVL(GeneroPelicula kv, AVL ti, AVL td) {
    GeneroPelicula kvi = ti->kv;
    AVL tii     = ti->left;
    AVL tid     = ti->right;
    int hii     = heightAVL(tii);
    int hid     = heightAVL(tid);
    if (hii >= hid) {
        return sJoinAVL(kvi, tii, sJoinAVL(kv, tid, td));
    }

    GeneroPelicula kvid = tid->kv;
    AVL tidi     = tid->left;
    AVL tidd     = tid->right;
    return sJoinAVL(kvid, sJoinAVL(kvi, tii, tidi), sJoinAVL(kv, tidd, td));
}

/// PRECOND: td es dos más profundo que ti (y por lo tanto es no vacio)
// Prop.: realiza una rotación a derecha
AVL rJoinAVL(GeneroPelicula kv, AVL ti, AVL td) {
    GeneroPelicula kvd = td->kv;
    AVL tdi     = td->left;
    AVL tdd     = td->right;
    int hdi = heightAVL(tdi);
    int hdd = heightAVL(tdd);
    if (hdi <= hdd) {
        return sJoinAVL(kvd, sJoinAVL(kv, ti, tdi), tdd);
    }

    GeneroPelicula kvdi = tdi->kv;
    AVL tdii = tdi->left;
    AVL tdid = tdi->right;
    return sJoinAVL(kvdi, sJoinAVL(kv, ti, tdii), sJoinAVL(kvd, tdid, tdd));
}

/**
PRECOND:
  * ti y td son BSTs
  * las claves de ti son menores que kv
  * las claves de td son mayores que kv
  * ti y td son AVLs
  * PERO ti y td pueden tener mas altura que lo necesario!!! (pero no deben!)
     (ojo: ti dos mas que td, o td dos mas que ti, pues antes eran AVLs...)
**/
// Prop.: realiza una rotación en base a las alturas
AVL joinAVL(GeneroPelicula c, AVL ti, AVL td) {
    int hi = heightAVL(ti);
    int hd = heightAVL(td);
    if (abs(hi-hd) <= 1) {
        return sJoinAVL(c, ti, td);
    } else if (hi == hd + 2) {
        return lJoinAVL(c, ti, td);
    } else if (hd == hi + 2) {
        return rJoinAVL(c, ti, td);
    }
    // nunca puede darse otro caso
    cout << "Se viola el invariante de representación!" << endl;
}

/// PRECOND: el AVL no está vacío
// Prop.: asigna el minimo al primer parametro y devuelve "t" sin ese minimo
AVL splitMaxAVL(GeneroPelicula& c, AVL t) {
    if(t->right == NULL) {
        GeneroPelicula cliente = t->kv;
        AVL left        = t->left;
        delete t->right;
        delete t;
        c  = cliente;
        return left;
    } else {
        AVL td = splitMaxAVL(c, t->right);
        return joinAVL(t->kv, t->left, td);
    }
}

///////////////////////////////////////////////
/// INTERFAZ DE MAP Y AUXILIARES
///////////////////////////////////////////////

/*
 *  Inv. Rep. del Map:
 *   - No tiene claves de kv repetidos
 *   - El map es AVL
 */

// Prop.: crea un Map vacio
// O(1) porque solo retorna null
Map emptyM()
{
    return NULL;
}

// Prop.: devuelve el GeneroPelicula
GeneroPelicula lookupM(Map& m, string genero)
{
    if(m==NULL)
    {
        return NULL;
    }
    if(m->kv->genero == genero)
    {
        return m->kv;
    }
    else if(genero < m->kv->genero)
         {
             return lookupM(m->left,genero);
         }
         else return lookupM(m->right,genero);
}
/*
// Prop.: asocia un key con un value
// O(log n) porque hago un recorrido logaritmico (en cada caso descarto la mitad)
void addM(Map& m, GeneroPelicula gp)
{
    if(m==NULL)
    {
        m = leafAVL(gp);
    }
    else if(gp->genero == m->kv->genero)
         {
             m->kv = gp;
         }
         else if(gp->genero<m->kv->genero)
              {
                  addM(m->left,gp);
              }
              else addM(m->right,gp);

    m = joinAVL(m->kv, m->left, m->right);
}*/

void addAux(Map& m, string genero, string pelicula)
{
    if(m==NULL)
    {
        GeneroPelicula ngp = new gpSt;
        ngp->genero = genero;
        ArrayList a = crearArrayList();
        add(a,pelicula);
        ngp->peliculas = a;
        m = leafAVL(ngp);
    }
    else if(genero == m->kv->genero)
         {
             m->kv->genero = genero;
             add(m->kv->peliculas,pelicula);
         }
         else if(genero<m->kv->genero)
              {
                  addAux(m->left,genero,pelicula);
              }
              else addAux(m->right,genero,pelicula);

    m = joinAVL(m->kv, m->left, m->right);
}

void addM(Map& m, ArrayList g, string nombre)
{
    for(int i = length(g)-1; i>=0; i--)
    {
        addAux(m,getAt(g,i),nombre);
    }
}

// Prop.: indica si la respuesta del lookup es válida
bool isNothing(GeneroPelicula gp)
{
    return gp == NULL;
}

// Prop.: Prop.: elimina un value dado una key
// O(log n) porque hago un recorrido logaritmico para encontrar al cliente y eliminarlo
void removeM(Map& m, string genero)
{
    if(m == NULL){
        return;
    }

    if(m->kv->genero == genero) {

        if(m->left == NULL) {
            Map temp = m;
            m = m->right;
            delete temp;
            return;
        }
        else {
            m = splitMaxAVL(m->kv, m);
        }
    }
    else if(m->kv->genero > genero) {

            removeM(m->left,genero);
         }
         else if(m->kv->genero < genero) {

                removeM(m->right,genero);
              }

    m = joinAVL(m->kv, m->left, m->right);
}

// Prop.: Agrega todos los cuits del map al array
// O(n) porque recorre todo el map y add es O(1) amortizado
void domAux(Map& m, ArrayList& res)
{
    if(m!=NULL)
    {
        add(res,m->kv->genero);
        domAux(m->left, res);
        domAux(m->right, res);
    }
}

// Prop.: devuelve la lista de claves de un Map
// O(n) porque recorre todos los elementos del map y los agrega a un array nuevo
ArrayList domM(Map& m)
{
    ArrayList res = crearArrayList();
    domAux(m, res);
    return res;
}

// Prop.: libera la memoria de un Map
// O(n) porque recorre y elimina todo el map
void destroyM(Map& m)
{
    if(m!=NULL)
    {
        destroyM(m->left);
        destroyM(m->right);
        delete m;
        m = NULL;
    }
}

///////////////////////////////////////////////
/// PRINT DEL MAP (para ver AVL como usuario)
/// NOTAR QUE ROMPE ENCAPSULAMIENTO
/// PERO AYUDA A VER EL ARBOL HASTA TENER
/// BIEN LA IMPLEMENTACION
///////////////////////////////////////////////

void emptySpace(int i) {
    for(; i > 0; i--) {
        cout << "-";
    }
}

void printMapAux(Map m, int i) {
    if(m == NULL) {
        emptySpace(i);
        cout << "NULL";
        return;
    }

    emptySpace(i);
    cout << "ROOT ";
    cout << m->kv->genero << "  ";
    imprimirArrayList(m->kv->peliculas);

    emptySpace(i);
    printMapAux(m->left, i+1);
    cout << endl;

    emptySpace(i);
    printMapAux(m->right, i+1);
    cout << endl;
}

void printMap(Map m) {
     printMapAux(m, 0);
}


#include "catamorphile.hpp"
#include "AtomIterators.h"

#include "Catamorphile_stub.h"

static PyObject* trampoline(PyObject* self, PyObject* args) {
  //SeleAtomIterator ai(nullptr, "");
  //SeleAtomIterator ai2(nullptr, "");
  test(0);
  test(0);
  return self;
}

static PyMethodDef CatamorphileMethods[] = {
    { "trampoline"
    , trampoline
    , METH_VARARGS
    , "Execute a shell command."
    },
    {NULL, NULL, 0, NULL}        /* Sentinel */
};

static struct PyModuleDef CatamorphileModule = {
    PyModuleDef_HEAD_INIT,
    "catamorphile",   /* name of module */
    NULL,
    -1,
    CatamorphileMethods
};

PyMODINIT_FUNC
PyInit_catamorphile(void)
{
    return PyModule_Create(&CatamorphileModule);
}

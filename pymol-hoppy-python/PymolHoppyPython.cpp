#include "PymolHoppyPython.hpp"

#include "AtomIterators.h"
#include "Cmd.h"
#include "HsFFI.h"
#include "PyMOLGlobals.h"
#include "PyMOLOptions.h"
#include "P.h"
#include "os_python.h"
#include "Selector.h"

#include "Data/Pymol/Extra_stub.h"

#define API_SETUP_ARGS(G, self, args, ...)				\
  if (!PyArg_ParseTuple(args, __VA_ARGS__))                                    \
    return nullptr;                                                            \
  G = _api_get_pymol_globals(self);					\
  API_ASSERT(G);

/**
 * If `x` is false, raises CmdException("x")
 */
#define API_ASSERT(x)                                                          \
  if (!(x)) {                                                                  \
    if (!PyErr_Occurred())                                                     \
      PyErr_SetString(P_CmdException ? P_CmdException : PyExc_Exception, #x);  \
    return nullptr;                                                            \
  }

static PyObject* trampoline(PyObject* self, PyObject* args) {
  //////////////////
  PyMOLGlobals *G = NULL;
  PyObject* pymol;
  char *str1;
  // int state;
  // char *ref_object;
  // int ref_state;
  OrthoLineType s1;
  PyObject* result = nullptr;

  if(!PyArg_ParseTuple(args, "Os", &pymol, &str1)) {
    PyErr_SetString(PyExc_Exception, "Expected (<pymol._cmd>, <selector>)");
  }
  G = _api_get_pymol_globals(pymol);
  API_ASSERT(G);

  // API_SETUP_ARGS(G, pymol, args, "Osisi", &pymol, &str1);

  // if(!ref_object[0])
  //  ref_object = NULL;
  APIEnterBlocked(G);
  int ok = SelectorGetTmp(G, str1, s1);
  SelectorID_t selectorId = SelectorIndexByName(G, str1, 1);
  SeleAtomIterator iterator(G, selectorId);

  if(ok >= 0) {
    result = PyLong_FromLong(test((HsInt) &iterator));
    // test((HsInt) &iterator);
    // test((HsInt) &iterator);
  }

  SelectorFreeTmp(G, s1);
  APIExitBlocked(G);
  
  return result;
}

static PyMethodDef PymolHoppyPythonMethods[] = {
    { "trampoline"
    , trampoline
    , METH_VARARGS
    },
    {NULL, NULL }        /* Sentinel */
};

static struct PyModuleDef PymolHoppyPythonModule = {
    PyModuleDef_HEAD_INIT,
    "pymol_hoppy_python",   /* name of module */
    NULL,
    -1,
    PymolHoppyPythonMethods
};

PyMODINIT_FUNC
PyInit_pymol_hoppy_python(void)
{
  char** argv = new char*[]{"pymol"};
  hs_init(0, &argv);
  return PyModule_Create(&PymolHoppyPythonModule);
}

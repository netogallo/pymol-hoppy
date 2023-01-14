#define PY_SSIZE_T_CLEAN
#include <Python.h>

static PyObject* trampoline(PyObject* self, PyObject* args);

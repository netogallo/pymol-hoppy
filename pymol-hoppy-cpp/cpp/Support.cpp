#include "Support.hpp"

#include <HsFFI.h>

/*
extern "C" {
extern void test(HsInt a1);
}


static PyObject* trampoline(PyObject* self, PyObject* args) {
  test(0);
  test(0);
  return self;
}
*/

SeleAtomIterator* newSelectionIterator() {

  return new SeleAtomIterator(nullptr, "");
}

#include "dummy/DummyAtomIterator.h"

void DummyAtomIterator::reset() {
  idx = -1;
}

bool DummyAtomIterator::next() {

  return ++idx < dummyMolecule.AtomInfo.size();
}

DummyAtomIterator::~DummyAtomIterator() {}

/*
  Todo:
  This segfaults because the ObjectMolecule class access fields
  of the PyMOLGlobals object in its constructor.

  Idea: Try running the function 'launch_library_singleton' beforehand
  and see if that can create an instance of the PyMOLGlobals w/o
  blowing up.
*/
DummyAtomIterator::DummyAtomIterator() : dummyMolecule(nullptr, 0) {

  AtomInfoType atomInfo;

  dummyMolecule.AtomInfo.resize(3);
}

#pragma once

#include "AtomIterators.h"
#include "ObjectMolecule.h"

class DummyAtomIterator : public AbstractAtomIterator {

public:
  void reset();

  bool next();

  DummyAtomIterator();
  
  ~DummyAtomIterator();

private:
  ObjectMolecule dummyMolecule;
};

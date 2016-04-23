#!/bin/bash
ghc -O2 -isrc -itest test/Spec.hs
test/Spec --match=$1

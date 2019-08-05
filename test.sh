#!/bin/bash
stack ghc -- -O2 -isrc -itest test/Spec.hs test/Main.hs
test/Main --match=$1

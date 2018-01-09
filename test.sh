#!/bin/bash
stack ghc -- -O2 -isrc -itest test/Spec.hs
dist/build/spec/spec --match=$1

#!/bin/bash
stack ghci \
  --ghci-options -isrc \
  --ghci-options -itest \
  --ghci-options -itest \
  --ghci-options -Wno-missing-home-modules \
  --ghci-options -Wno-unused-packages \
  --ghci-options -Wno-unused-packages \
  --ghci-options "-package hspec" \
  --ghci-options "-package QuickCheck" \
  --ghci-options "-package tasty" \
  --ghci-options "-package tasty-hspec" \
  --ghci-options "-package HUnit" \
  --ghci-options "-package random" \
  --ghci-options "-package tasty-hunit" \
  --ghci-options "-package tasty-quickcheck" \
  --ghci-options "-Wno-type-defaults" \
  --ghci-options "-fobject-code" \
  --ghci-options "-O2" \
  --main-is hilbert:test:hilbert-tests

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
  --ghci-options "-Wno-type-defaults" \
  --main-is hilbert:test:hilbert-tests

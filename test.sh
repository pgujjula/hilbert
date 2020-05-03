#!/bin/bash
stack runhaskell -- -isrc -itest test/Main.hs --match=$1

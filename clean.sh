#!/bin/bash
# clean all *.o and *.hi files
find ./ -type f -name "*.o" -exec rm -v {} +
find ./ -type f -name "*.hi" -exec rm -v {} +

#!/bin/bash

rm -f res
awk '/^COMPARISON/{getline;getline; print;getline;print}' LOG | \
    tr -d '[\[,\]]' > res

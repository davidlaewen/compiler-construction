#!/bin/bash

stack build;

for file in tests-codegen/*.spl; do
    echo "-------------------------------------";
    echo $file;
    stack run -- parse $file 1>/dev/null;
done

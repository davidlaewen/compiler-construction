#!/bin/bash

stack build;

for file in tests-codegen/*.spl; do
    echo "-------------------------------------";
    echo $file;
    stack run -- codegen $file 1> output.ssm;
    java -jar ./../ssm/ssm.jar $* --file output.ssm --cli;
done

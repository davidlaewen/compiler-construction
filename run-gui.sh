#!/bin/bash

stack build

file=$1; # Take file path as command line arg
stack run -- codegen $file > output.ssm;
java -jar ./../ssm/ssm.jar --file output.ssm;

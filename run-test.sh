#!/bin/bash

stack build

file=tests-codegen/tuple-passing.spl;
stack run -- codegen $file > output.ssm;
java -jar ./../ssm/ssm.jar $* --file output.ssm --cli;

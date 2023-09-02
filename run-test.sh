#!/bin/bash

stack build

file=tests-codegen/codegen.spl;
stack run -- codegen $file > output.ssm;
java -jar ./../ssm/ssm.jar $* --file output.ssm --cli;

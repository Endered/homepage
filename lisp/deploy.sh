#!/usr/bin/env bash

target=$1
output=$2

cat $target | gosh header.scm | gosh header-js.scm | gosh simplify.scm | gosh cps.scm | gosh js.scm > $output

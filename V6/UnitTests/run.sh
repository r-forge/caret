#!/bin/bash

R --slave -f runUnitTests.R --args "$@" > testrun.out 2>&1

#!/bin/bash
curl https://adventofcode.com/2020/day/$1/input --cookie "session=`cat session.txt`" > day$1.txt

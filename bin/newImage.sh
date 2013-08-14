#!/bin/bash
NAME="$1"
mkdir $NAME
cd $NAME
../Cuis-Smalltalk-Dev/bin/copyImage.sh $NAME Cuis4.2-1798
../Cuis-Smalltalk-Dev/bin/getCog.sh 13.30 2761

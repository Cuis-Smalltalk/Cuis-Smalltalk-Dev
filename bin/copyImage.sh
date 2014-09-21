#!/bin/bash
NEW="$1"
cp Cuis-Smalltalk-Dev/Cuis*.image .
cp Cuis-Smalltalk-Dev/Cuis*.changes .
cp Cuis-Smalltalk-Dev/CuisV4.sources .
mv Cuis*.image $NEW.image
mv Cuis*.changes $NEW.changes
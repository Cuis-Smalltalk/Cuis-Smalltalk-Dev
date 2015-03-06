#!/bin/bash
NEW="$1"
cp Cuis-Smalltalk-Dev/Cuis4.2-2203.image .
cp Cuis-Smalltalk-Dev/Cuis4.2-2203.changes .
cp Cuis-Smalltalk-Dev/CuisV4.sources .
mv Cuis*.image $NEW.image
mv Cuis*.changes $NEW.changes

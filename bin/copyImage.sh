#!/bin/bash
NEW="$1"
OLD="$2"
cp ../Cuis-Smalltalk-Dev/$OLD.image .
cp ../Cuis-Smalltalk-Dev/$OLD.changes .
cp ../Cuis-Smalltalk-Dev/CuisV4.sources .
mv $OLD.image $NEW.image
mv $OLD.changes $NEW.changes
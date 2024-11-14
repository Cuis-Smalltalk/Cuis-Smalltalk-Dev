#!/usr/bin/env bash
# File:        			unquarantine.sh
# Author:      			Juan Vuletich
# Description: Remove apple's quarantine attribute to files, to prevent App Translocation

# Needed if you get a debugger and a message like this in the Transcript.
# Could not write to changes file: /private/var/folders/q7/6m669tws3y9ds2kkcydtjmh00000gp/T/AppTranslocation/18B88660-C5DC-492F-83AA-02A9B9F3CA13/d/Cuis.app/Contents/Resources/Cuis7.0.changes

xattr -dr com.apple.quarantine *

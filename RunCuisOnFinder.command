#!/bin/bash
CTWN="Cuis Terminal Window $$"
echo $CTWN
echo -n -e "\033]0;$CTWN\007"
if which -s brew; then
	export DYLD_LIBRARY_PATH="$(brew --prefix)/lib:${DYLD_LIBRARY_PATH}"
fi
$(/usr/bin/dirname "$0")/CuisVM.app/Contents/MacOS/Squeak $(/usr/bin/dirname "$0")/CuisImage/Cuis?.?-????.image -u &
osascript -e "tell application \"Terminal\" to close (every window whose name contains \"$CTWN\")" &

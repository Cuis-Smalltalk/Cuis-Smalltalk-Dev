if which -s brew; then
	export DYLD_LIBRARY_PATH="$(brew --prefix)/lib:${DYLD_LIBRARY_PATH}"
fi
if [ "$1" = "-vm" ]; then
	VM="$2"; shift; shift
else
	VM=./CuisVM.app/Contents/MacOS/Squeak
fi
"$VM" CuisImage/Cuis?.?-????.image -u "$@"

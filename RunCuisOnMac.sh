if which -s brew; then
	export DYLD_LIBRARY_PATH="$(brew --prefix)/lib:${DYLD_LIBRARY_PATH}"
fi
./CuisVM.app/Contents/MacOS/Squeak CuisImage/Cuis?.?-????.image -u "$@"

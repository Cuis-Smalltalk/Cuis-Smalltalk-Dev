#!/bin/bash

if [ ! -d "$1" ]; then
	echo Bad home directory.
	exit 1
fi

if [ -z "$(find "$1/.." -name "Cuis*.image")" ]; then
	echo Home directory missing image file\(s\).
	exit 1
fi

if [ -z "$(find "$1/.." -name "Cuis*.changes")" ]; then
	echo Home directory missing change file\(s\).
	exit 1
fi

if [ ! -e "$1/../CuisV4.sources" ]; then
	echo Home directory missing source file.
	exit 1
fi

if [ ! -d "$2" ]; then
	echo "$2"
	echo Bad image directory.
	exit 1
fi

if [ -z "$3" ]; then
	echo Missing image name.
	exit 1
fi

homeDirectory="$1"
imageDirectory="$2"
imageName="$3"

imageToCopy=$(ls -1 "$homeDirectory"/../Cuis*-*image | sort -rnk 2 -t - | head -n 1)
changesToCopy=$(ls -1 "$homeDirectory"/../Cuis*-*changes | sort -rnk 2 -t - | head -n 1)

if [ ! -e "$imageToCopy" ]; then
	echo Missing image file.
	exit 1
fi

if [ ! -e "$changesToCopy" ]; then
	echo Missing changes file.
	exit 1
fi

cp "$imageToCopy" "$imageDirectory/$imageName.image"
cp "$changesToCopy" "$imageDirectory/$imageName.changes"
cp "$homeDirectory"/../CuisV4.sources "$imageDirectory"/.

exit 0

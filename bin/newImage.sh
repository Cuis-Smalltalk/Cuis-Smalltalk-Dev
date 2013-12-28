#!/bin/bash

homeDirectory=$(cd `dirname "$0"` && pwd)

if [ -z "$1" ]; then
	echo No image path provided.
	exit 1
fi

imageDirectory=$(dirname "$1")
imageName=$(basename "$1")

if [ -d "$imageDirectory" ]; then
	echo Image directory already exists.
	exit 1
fi

if [ -z "$imageName" ]; then
	echo Bad image name.
	exit 1
fi

mkdir -p "$imageDirectory"

if [ ! -d "$imageDirectory" ]; then
	echo Failed creating image directory.
	exit 1
fi

"$homeDirectory"/copyImage.sh "$homeDirectory" "$imageDirectory" "$imageName"

if [ $? -ne 0 ]; then
	exit 1
fi

cd "$imageDirectory"
"$homeDirectory"/getCog.sh 13.33 2776


#!/bin/bash
VERSION="$1"
BUILD="$2"
case "$OSTYPE" in
	darwin*) NAME="Cog.app" ;;
	*) NAME="coglinux" ;;
esac
curl http://www.mirandabanda.org/files/Cog/VM/2014/VM.r$BUILD/$NAME-$VERSION.$BUILD.tgz -o Cog.$BUILD.tgz
tar -xf Cog.$BUILD.tgz
rm Cog.$BUILD.tgz

#!/bin/bash
VERSION="$1"
BUILD="$2"
curl http://www.mirandabanda.org/files/Cog/VM/VM.r$BUILD/Cog.app-$VERSION.$BUILD.tgz -o Cog.$BUILD.tgz
tar -xf Cog.$BUILD.tgz
mv Cog.app/ Cog.$BUILD.app
rm Cog.$BUILD.tgz
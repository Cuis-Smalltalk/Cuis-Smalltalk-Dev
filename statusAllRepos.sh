#!/bin/bash

repositories="Cuis-Smalltalk-Dev GUIDesigner Cuis-Smalltalk-Tools SVG Calendars Measures Cuis-Smalltalk-UI Numerics CodeExamples AnimatedGIF Cuis-Smalltalk-Regex Parsers OSProcess DatabaseSupport GeographicInformationSystems Morphic Games Erudite StyledTextEditor VMMaker"

echo
echo -e "Status of \e[7m =====Cuis-Smalltalk-Dev===== \e[0m"
git status
echo
cd ..
for repository in $repositories;
do
    if test -r $repository
    then
	echo -e "Status of \e[7m -----$repository----- \e[0m..."
	cd $repository
	git status -s
	cd ..
    else
	echo "Repository $repository not cloned"
    fi
done

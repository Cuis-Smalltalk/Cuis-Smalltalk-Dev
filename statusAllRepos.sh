#!/bin/bash

repositories="VectorGraphics EnhancedText Erudite StyledTextEditor Measures Calendars CodeExamples Games Morphic Cairo OSProcess Numerics GeographicInformationSystems Parsers Machine-Learning AMQP firmata Learning-Cuis TheCuisBook"

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

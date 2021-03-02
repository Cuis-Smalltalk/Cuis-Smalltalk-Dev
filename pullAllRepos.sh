#!/bin/bash

repositories="SVG EnhancedText Erudite StyledTextEditor Measures Calendars CodeExamples Games Morphic Cairo OSProcess Numerics GeographicInformationSystems Parsers Machine-Learning AMQP firmata VMMaker Learning-Cuis TheCuisBook"

echo -e "Pulling \e[7m =====Cuis-Smalltalk-Dev===== \e[0m"
git pull
cd ..
for repository in $repositories;
do
    if test -r $repository
    then
	echo -e "Pulling \e[7m -----$repository----- \e[0m..."
	cd $repository
	git pull
	cd ..
    else
	echo "Repository $repository not cloned"
    fi
done

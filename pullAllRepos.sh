#!/bin/bash

repositories="AMQP AnimatedGIF Cairo Calendars CodeExamples Cuis-Smalltalk-Tools Cuis-Smalltalk-UI EnhancedText Erudite firmata Games GeographicInformationSystems Learning-Cuis Machine-Learning Measures Morphic Numerics OSProcess Parsers StyledTextEditor SVG VMMaker TheCuisBook Cuis-Smalltalk.github.io Cuis-Website Cuis-Smalltalk-Historical Cuis-Smalltalk-Regex"

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

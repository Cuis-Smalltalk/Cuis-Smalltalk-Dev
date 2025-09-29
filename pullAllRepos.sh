#!/bin/bash

repositories="CodeExamples Calendars Measures Cuis-Smalltalk-UI Numerics GeographicInformationSystems SVG AnimatedGIF Parsers Cuis-Smalltalk-Regex Cuis-Smalltalk-Tools DatabaseSupport OSProcess AMQP firmata Machine-Learning Cairo Morphic EnhancedText Games Erudite StyledTextEditor VMMaker Cuis-Website Cuis-Smalltalk.github.io Cuis-Smalltalk-Historical"

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

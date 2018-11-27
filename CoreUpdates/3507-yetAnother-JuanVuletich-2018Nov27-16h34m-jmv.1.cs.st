'From Cuis 5.0 of 7 November 2016 [latest update: #3505] on 27 November 2018 at 4:39:19 pm'!

!PluggableTextModel commentStamp: '<historical>' prior: 0!
A TextModel whose contents are stored elsewhere (ivar textProvider)!


!TextProvider methodsFor: 'accessing' stamp: 'jmv 11/27/2018 16:38:04'!
methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^[ UndefinedObject methodNodeFor: aSourceCode noPattern: true ] on: Error, UndeclaredVariableReference do: aParsingErrorBlock
! !


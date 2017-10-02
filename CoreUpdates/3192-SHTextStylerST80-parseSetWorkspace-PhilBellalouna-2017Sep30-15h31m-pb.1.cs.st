'From Cuis 5.0 of 7 November 2016 [latest update: #3186] on 30 September 2017 at 3:32:06 pm'!

!SHTextStylerST80 methodsFor: 'private' stamp: 'pb 9/30/2017 15:31:56'!
parseSetWorkspace: aBoolean
	"Answer a collection of SHRanges by parsing aText.
	When formatting it is not necessary to set the workspace, and this can make the parse take less time, so aBoolean specifies whether the parser should be given the workspace"
	parser ifNil: [ parser := SHParserST80 new ].
	parser
		workspace:
			(aBoolean ifTrue: [ workspace ]);
		classOrMetaClass: classOrMetaClass;
		source: formattedText asString.
	parser parse.
	^ parser ranges.! !


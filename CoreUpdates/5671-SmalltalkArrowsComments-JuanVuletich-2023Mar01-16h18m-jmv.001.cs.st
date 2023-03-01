'From Cuis 6.0 [latest update: #5670] on 1 March 2023 at 4:32:52 pm'!

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 3/1/2023 16:21:27'!
smalltalkDownArrow
	"
	The use of code 31 for a down arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text.
	Cuis holds a down arrow glyph at this code. This is for possible future use.

	Character smalltalkDownArrow
	"
	^ Character numericValue: 31! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 3/1/2023 16:25:16'!
smalltalkLeftArrow
	"
	The use of code 28 for a left arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text files.
	Cuis holds a left arrow glyph at this code, that is only used to show $_ as a left arrow,
	when used for assignment of a Smalltalk variable.
	Note that it will be replaced by $_ when saving code files or exporting to OS clipboard.

	Character smalltalkLeftArrow
	"
	^ Character numericValue: 28! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 3/1/2023 16:21:56'!
smalltalkRightArrow
	"
	The use of code 30 for a right arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text.
	Cuis holds a right arrow glyph at this code. This is for possible future use.

	Character smalltalkRightArrow
	"
	^ Character numericValue: 30! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 3/1/2023 16:25:09'!
smalltalkUpArrow
	"
	The use of code 29 for an up arrow is not part of the ASCII or Unicode standards.
	This code is generally not used in text files.
	Cuis holds an up arrow glyph at this code, that is only used to show $^ as an up arrow,
	when used for return from a Smalltalk method.
	Note that it will be replaced by $^ when saving code files or exporting to OS clipboard.

	Character smalltalkUpArrow
	"
	^ Character numericValue: 29! !


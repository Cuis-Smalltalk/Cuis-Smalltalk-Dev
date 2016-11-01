'From Cuis 4.2 of 25 July 2013 [latest update: #2940] on 14 October 2016 at 5:43:28 pm'!
!classDefinition: #Character category: #'Kernel-Text'!
Object immediateSubclass: #Character
	instanceVariableNames: ''
	classVariableNames: 'CharacterTable ClassificationTable LetterBits LowercaseBit UnaccentedTable UnicodeCodePoints UppercaseBit Space Tab Lf Cr '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!Character class methodsFor: 'accessing untypeable characters' stamp: 'pb 10/14/2016 17:43:07'!
cr
	"Answer the Character representing a carriage return."

	^ Cr! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'pb 10/14/2016 17:43:00'!
lf
	"Answer the Character representing a linefeed."

	^ Lf! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'pb 10/14/2016 17:42:52'!
space
	"Answer the Character representing a space."

	^ Space! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'pb 10/14/2016 17:42:45'!
tab
	"Answer the Character representing a tab."

	^ Tab! !

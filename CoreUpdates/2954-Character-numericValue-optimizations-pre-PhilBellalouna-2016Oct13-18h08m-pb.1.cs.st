'From Cuis 4.2 of 25 July 2013 [latest update: #2940] on 14 October 2016 at 5:43:28 pm'!
!classDefinition: #Character category: #'Kernel-Text'!
Object immediateSubclass: #Character
	instanceVariableNames: ''
	classVariableNames: 'CharacterTable ClassificationTable LetterBits LowercaseBit UnaccentedTable UnicodeCodePoints UppercaseBit Space Tab Lf Cr '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!Character class methodsFor: 'class initialization' stamp: 'pb 10/14/2016 17:41:16'!
initClassCachedState
	"Create the table of unique Characters.
	Character initialize
	"
	Cr := Character numericValue: 13.
	Lf := Character numericValue: 10.
	Tab := Character numericValue: 9.
	Space := Character numericValue: 32.
	self initializeClassificationTable.
	self initializeUnicodeCodePoints! !

!Character methodsFor: 'testing' stamp: 'pb 10/13/2016 18:16:24'!
isDigit
	"Answer whether the receiver is a digit."
	|numericVal|
	numericVal := self numericValue.
	^ numericVal >= 48 and: [numericVal <= 57]! !

!Character methodsFor: 'testing' stamp: 'pb 10/13/2016 18:10:29'!
isLineSeparator
	"Answer whether the receiver is a line separator character:
	cr, line feed, or form feed."
	|numericVal|
	numericVal := self numericValue.
	numericVal = 10 ifTrue: [^true].	"line feed"
	numericVal = 13 ifTrue: [^true].	"cr"
	numericVal = 12 ifTrue: [^true].	"form feed"
	^false! !

!Character methodsFor: 'testing' stamp: 'pb 10/13/2016 18:09:18'!
isSeparator
	"Answer whether the receiver is one of the separator characters--space, 
	cr, tab, line feed, or form feed."
	|numericVal|
	numericVal := self numericValue.
	numericVal = 32 ifTrue: [^true].	"space"
	numericVal = 13 ifTrue: [^true].	"cr"
	numericVal = 9 ifTrue: [^true].		"tab"
	numericVal = 10 ifTrue: [^true].	"line feed"
	numericVal = 12 ifTrue: [^true].	"form feed"
	^false! !

!Character methodsFor: 'accessing' stamp: 'pb 10/13/2016 18:10:00'!
digitValue
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 
	otherwise. This is used to parse literal numbers of radix 2-36."
	| aNumericVal numericVal |
	numericVal := self numericValue.
	numericVal <= $9 numericValue ifTrue: [ ^ numericVal - $0 numericValue ].
	aNumericVal := $A numericValue.
	numericVal >= aNumericVal ifTrue: [
		numericVal <= $Z numericValue ifTrue: [ ^ numericVal - aNumericVal + 10 ]].
	^ -1.! !

Character initialize!
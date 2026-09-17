'From Cuis7.9 [latest update: #8199] on 17 September 2026 at 1:03:32 pm'!

!Parser methodsFor: 'scanning' stamp: 'jmv 17/Sep/2026, 3:56:53 pm (UTC)'!
endOfLastMethodHeader
	"Include keywords and arguments"

	"Unary and Binary selectors"
	prevMark = 1 ifTrue: [ ^hereEnd ].

	"Keyword selectors"
	^ self endOfLastToken! !


!Parser methodsFor: 'private' stamp: 'jmv 17/Sep/2026, 3:22:15 pm (UTC)'!
privateReadSelector
	| args selector |
	doitFlag := false.

	hereType == #word ifTrue: [
		^ here asSymbol ].

	self transformVerticalBarAndUpArrowIntoABinarySelector.

	hereType == #binary ifTrue: [
		selector := self advance asSymbol.
		^selector ].

	hereType == #keyword ifTrue: [
		selector := source collectionSpecies writeStream.
		args := OrderedCollection new.
		[hereType == #keyword] whileTrue: [
			selector nextPutAll: self advance.
			args addLast: (encoder bindArg: self argumentName).
		].
		^ selector contents asSymbol ].

	^self expected: self messagePatternExpectedErrorMessage! !


!Parser class methodsFor: 'services' stamp: 'jmv 17/Sep/2026, 3:56:55 pm (UTC)'!
methodHeaderLengthFrom: methodSource

	| parser |
	parser := self new.
	parser privateReadSelectorFrom: methodSource.
	^ parser endOfLastMethodHeader! !


!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 17/Sep/2026, 3:57:36 pm (UTC)'!
methodHeaderFor: selector 
	"Answer the string corresponding to the method header for the given selector.
	This is, the selector and any parameters."

	| methodSource |
	methodSource := self ultimateSourceCodeAt: selector ifAbsent: [self standardMethodHeaderFor: selector].
	^methodSource copyFrom: 1 to: (self parserClass methodHeaderLengthFrom: methodSource)

"
Behavior methodHeaderFor: #methodHeaderFor:
Behavior methodHeaderFor: #highestClassImplementing:ifNone:
Object methodHeaderFor: #at:
Object methodHeaderFor: #at:put:

Collection methodHeaderFor: #,
Number methodHeaderFor: #//
Object methodHeaderFor: #~=

Object methodHeaderFor: #basicSize
Number methodHeaderFor: #asFloat
DifferenceFinder class methodHeaderFor: #l
"
! !


'From Cuis 4.2 of 25 July 2013 [latest update: #2022] on 29 June 2014 at 7:52:50.775744 pm'!

!Parser methodsFor: 'private' stamp: 'jmv 6/29/2014 19:50'!
privateReadSelector
	| args selector |
	doitFlag := false.

	hereType == #word ifTrue: [
		^ here asSymbol ].

	hereType == #binary ifTrue: [
		^ here asSymbol ].

	hereType == #keyword ifTrue: [
		selector := WriteStream on: (String new: 32).
		args := OrderedCollection new.
		[hereType == #keyword] whileTrue: [
			selector nextPutAll: self advance.
			args addLast: (encoder bindArg: self argumentName).
		].
		^ selector contents asSymbol ].

	^self expected: 'Message pattern'! !

!Parser methodsFor: 'private' stamp: 'jmv 6/29/2014 19:51'!
privateReadSelectorFrom: aMethodSource
	"Answer the message selector for the argument, aMethodSource, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."
	"Note: only intended to read the selector. Parser instance might be inconsistent afterwards.
	Optimized these kind of methods (compare the following):
	[ 100000 timesRepeat: [Parser new parseSelector: 'a ^#[123 123 123 123 123 123 123 123 987 987 987 987 987 987 987 987]'] ] timeToRun 4824
	[ 100000 timesRepeat: [Parser new privateReadSelectorFrom: 'a ^#[123 123 123 123 123 123 123 123 987 987 987 987 987 987 987 987]'] ] timeToRun  342
	"

	| result |
	self initScannerForTokenization.
	self
		init: (ReadStream on: aMethodSource asString)
		notifying: nil
		failBlock: [ ^nil ].
	encoder _ self.
	result _ self privateReadSelector.
	encoder _ failBlock _ nil.  "break cycles"
	^result! !


!Parser class methodsFor: 'services' stamp: 'jmv 6/29/2014 19:51'!
selectorFrom: methodSource

	^self new privateReadSelectorFrom: methodSource! !


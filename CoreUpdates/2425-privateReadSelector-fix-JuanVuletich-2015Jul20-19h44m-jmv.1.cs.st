'From Cuis 4.2 of 25 July 2013 [latest update: #2424] on 20 July 2015 at 7:48:35.875502 pm'!

!Parser methodsFor: 'private' stamp: 'jmv 7/20/2015 19:45'!
privateReadSelector
	| args selector |
	doitFlag := false.

	hereType == #word ifTrue: [
		^ here asSymbol ].

	hereType == #binary ifTrue: [
		^ here asSymbol ].

	hereType == #verticalBar ifTrue: [
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


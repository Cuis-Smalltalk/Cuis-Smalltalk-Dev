'From Cuis 5.0 of 7 November 2016 [latest update: #3418] on 15 August 2018 at 7:55:52 pm'!

!Scanner methodsFor: 'initialization' stamp: 'HAW 8/15/2018 19:55:10'!
ignoreBacktick: aBoolean

	ignoreBacktick := aBoolean ! !


!Scanner methodsFor: 'expression types' stamp: 'HAW 8/15/2018 19:55:41'!
scanToken

	"Skip delimiters fast, there almost always is one."
	self skipDelimitersAndBacktickIfNecessary.

	mark := source position - 1.
	(tokenType at: 1) = $x "x as first letter"
		ifTrue: [self perform: tokenType "means perform to compute token & type"]
		ifFalse: [token := self step asSymbol "else just unique the first char"].
	^token! !


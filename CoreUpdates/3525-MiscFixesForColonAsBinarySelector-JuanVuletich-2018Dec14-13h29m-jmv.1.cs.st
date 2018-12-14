'From Cuis 5.0 of 7 November 2016 [latest update: #3524] on 14 December 2018 at 3:43:44 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 12/14/2018 15:30:45'!
isValidInBinarySelectors
	"Can be part of a binary selector?
		$< isValidInBinarySelectors
		$| isValidInBinarySelectors
		$^ isValidInBinarySelectors
		$: isValidInBinarySelectors
	"
	^#(verticalBar upArrow xColon xBinary) statePointsTo: (Scanner typeTable at: self numericValue)! !


!String methodsFor: 'converting' stamp: 'jmv 12/14/2018 15:40:54'!
findSelector
	"Revised to use scanner for better removal of extraneous stuff"
	| sel colonIndex |
	sel _ self withBlanksTrimmed.
	colonIndex _ sel indexOf: $:.
	"possible keyword selector"
	(colonIndex > 1 and: [ (self at: colonIndex - 1) isLetter ]) ifTrue: [
		sel _ Scanner findSelectorIn: sel ].
	sel isEmpty ifTrue: [ ^ nil ].
	Symbol
		hasInterned: sel
		ifTrue: [ :aSymbol |
			^ aSymbol ].
	^ nil.! !

!String methodsFor: 'converting' stamp: 'jmv 12/14/2018 09:53:01'!
keywords
	"Answer an array of the keywords that compose the receiver."
	| kwd char keywords isAllLeters |
	keywords _ Array streamContents: [ :kwds |
		kwd _ WriteStream on: (String new: 16).
		isAllLeters _ true.
		1
			to: self size
			do: [ :i |
				char _ self at: i.
				kwd nextPut: char.
				char = $: & isAllLeters
					ifTrue: [
						kwds nextPut: kwd contents.
						kwd reset.
						isAllLeters _ true ]
					ifFalse: [
						char isLetter ifFalse: [ isAllLeters _ false ]]].
		kwd isEmpty ifFalse: [ kwds nextPut: kwd contents ]].
	
	^ keywords.! !

!String methodsFor: 'system primitives' stamp: 'jmv 12/14/2018 15:27:30'!
numArgs
	"Answer either the number of arguments that the receiver would take if considered a selector. 
	Answer -1 if it couldn't be a selector. 
		Note that currently this will answer -1 for anything begining with an uppercase letter even though 
		the system will accept such symbols as selectors.  It is intended mostly for the assistance of spelling correction."
	| firstChar numColons start ix |
	self size = 0 ifTrue: [ ^ -1 ].
	"Binary messages"
	(self allSatisfy: [ :c | c isValidInBinarySelectors ])
		ifTrue: [ ^1 ].
	"Unary and keyword messages"
	firstChar _ self at: 1.
	firstChar isValidStartOfIdentifiers ifTrue: [
		"Fast reject if any chars are non-alphanumeric"
		(self
			findSubstring: '~'
			in: self
			startingAt: 1
			matchTable: Tokenish) > 0 ifTrue: [ ^ -1 ].
		"Fast colon count"
		numColons _ 0.
		start _ 1.
		[
		(ix _ self
			findSubstring: ':'
			in: self
			startingAt: start
			matchTable: CaseSensitiveOrder) > 0 ] whileTrue: [
			numColons _ numColons + 1.
			start _ ix + 1 ].
		^ numColons ].
	^ -1.! !


!Symbol methodsFor: 'accessing' stamp: 'jmv 12/14/2018 15:41:59'!
precedence
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise.  The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	| c |
	self size = 0 ifTrue: [^ 0].
	"Consider selectors starting with an underscore $_ as unary, even if Preferences allowUnderscoreSelectors is not set."
	c _ self first.
	c isValidInBinarySelectors ifTrue: [^ 2].
	self last = $: ifTrue: [^ 3].
	^ 1! !


!SHParserST80 methodsFor: 'character testing' stamp: 'jmv 12/14/2018 15:39:18'!
isBinarySelectorCharacter: aCharacter
	aCharacter = $: ifTrue: [^ false].
	^aCharacter isValidInBinarySelectors! !


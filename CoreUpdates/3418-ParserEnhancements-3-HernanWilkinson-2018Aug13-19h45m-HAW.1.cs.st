'From Cuis 5.0 of 7 November 2016 [latest update: #3411] on 13 August 2018 at 7:48:17 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 8/13/2018 19:47:02'!
blockExpression
	"[ ({:var} |) (| {temps} |) (statements) ] => BlockNode."

	| blockNode variableNodes temporaryBlockVariables start |
	blockNode := BlockNode new.
	variableNodes := OrderedCollection new.
	start := prevMark + requestorOffset.
	"Gather parameters."
	[self match: #colon] whileTrue:
		[self argumentNameWithRangeDo: [ :argumentName :range |
			variableNodes addLast: (encoder bindBlockArg: argumentName within: blockNode range: range)]].
	(variableNodes size > 0 & (hereType ~~ #rightBracket) and: [(self match: #verticalBar) not]) ifTrue:
		[^self expected: 'Vertical bar'].

	temporaryBlockVariables := self temporaryBlockVariablesFor: blockNode.
	self statements: variableNodes innerBlock: true blockNode: blockNode.
	blockNode temporaries: temporaryBlockVariables.

	(self match: #rightBracket) ifFalse: [^self expected: 'Period or right bracket'].

	blockNode noteSourceRangeStart: start end: self endOfLastToken encoder: encoder.

	"The scope of the parameters and temporary block variables is no longer active."
	temporaryBlockVariables do: [:variable | variable scope: -1].
	variableNodes do: [:variable | variable scope: -1]! !

!Parser methodsFor: 'expression types' stamp: 'HAW 8/13/2018 19:47:49'!
temporaryBlockVariablesFor: aBlockNode
	"Scan and answer temporary block variables."

	| variables |
	(self match: #verticalBar) ifFalse:
		"There are't any temporary variables."
		[aBlockNode tempsMark: prevMark + requestorOffset.
		 ^#()].

	variables := OrderedCollection new.
	[hereType == #word] whileTrue:
		[self advanceWithRangeDo: [ :lexema :range |
			variables addLast: (encoder bindBlockTemp: lexema within: aBlockNode range: range)]].
	(self match: #verticalBar) ifFalse:
		[^self expected: 'Vertical bar'].
	aBlockNode tempsMark: prevMark + requestorOffset.
	^variables! !


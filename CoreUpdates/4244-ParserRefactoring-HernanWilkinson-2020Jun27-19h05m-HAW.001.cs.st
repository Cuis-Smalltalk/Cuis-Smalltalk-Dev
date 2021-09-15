'From Cuis 5.0 [latest update: #4242] on 27 June 2020 at 7:07:50 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 6/27/2020 19:06:38'!
	blockExpression
		"[ ({:var} |) (| {temps} |) (statements) ] => BlockNode."

		| blockNode tempsDeclarationNode variableNodes temporaryBlockVariables start |

		advanced == true ifFalse: [ self advance ].

		blockNode := BlockNode new.
		variableNodes := OrderedCollection new.
		start := prevMark + requestorOffset.
		"Gather parameters."
		[self match: #colon] whileTrue:
			[self argumentNameWithRangeDo: [ :argumentName :range |
				variableNodes addLast: (encoder bindBlockArg: argumentName within: blockNode range: range)]].
		(variableNodes size > 0 & (hereType ~~ #rightBracket) and: [(self match: #verticalBar) not]) ifTrue:
			[^self expected: 'Vertical bar'].

		tempsDeclarationNode := self temporaryBlockVariablesFor: blockNode.
		temporaryBlockVariables := tempsDeclarationNode allDeclaredVariableNodes.
		self statements: variableNodes innerBlock: true blockNode: blockNode.
		blockNode temporariesDeclaration: tempsDeclarationNode.

		(self match: #rightBracket) ifFalse: [^self expected: 'Period or right bracket'].

		blockNode noteSourceRangeStart: start end: self endOfLastToken encoder: encoder.

		"The scope of the parameters and temporary block variables is no longer active."
		temporaryBlockVariables do: [:variable | variable scope: -1].
		variableNodes do: [:variable | variable scope: -1].

		^true! !

'From Cuis 5.0 [latest update: #4242] on 27 June 2020 at 7:14:34 pm'!
!classDefinition: #Parser category: #'Compiler-Kernel'!
Scanner subclass: #Parser
	instanceVariableNames: 'here hereType hereMark hereEnd prevMark prevEnd encoder requestor parseNode failBlock requestorOffset tempsMark doitFlag properties category advanced '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Parser methodsFor: 'expression types' stamp: 'HAW 6/27/2020 19:13:39'!
blockExpression
	"[ ({:var} |) (| {temps} |) (statements) ] => BlockNode."

	| blockNode tempsDeclarationNode variableNodes temporaryBlockVariables start |

	self advance.
	
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

!classDefinition: #Parser category: #'Compiler-Kernel'!
Scanner subclass: #Parser
	instanceVariableNames: 'here hereType hereMark hereEnd prevMark prevEnd encoder requestor parseNode failBlock requestorOffset tempsMark doitFlag properties category'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

'From Cuis 5.0 [latest update: #4059] on 13 March 2020 at 12:25:29 am'!
"Change Set:		4060-CuisCore-AuthorName-2020Mar13-00h15m
Date:			13 March 2020
Author:			Nahuel Garbezza

Allow Parser to parse temp declaration nodes. It has some code that needs to be cleaned up after integrating this change"!


!Parser methodsFor: 'temps' stamp: 'RNG 3/13/2020 00:16:54'!
createTempDeclarationOf: variableNode sourceRange: sourceRange

	| declarationNode |
	declarationNode _ TemporaryDeclarationNode of: variableNode.
	encoder noteSourceRange: sourceRange forNode: declarationNode.
	^ declarationNode! !

!Parser methodsFor: 'temps' stamp: 'RNG 3/13/2020 00:17:01'!
createTempsDeclarationWith: tempDeclarationNodes sourceRange: sourceRange

	|tempsDeclarationNode|
	tempsDeclarationNode _ TemporariesDeclarationNode withAll: tempDeclarationNodes.
	encoder noteSourceRange: sourceRange forNode: tempsDeclarationNode.
	^ tempsDeclarationNode! !

!Parser methodsFor: 'temps' stamp: 'RNG 3/13/2020 00:17:09'!
noTempsDeclaration

	^ TemporariesDeclarationNode empty! !


!Parser methodsFor: 'expression types' stamp: 'RNG 3/13/2020 00:23:38'!
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

	"RNG - temporary change until we fully start using temporary declaration nodes in the parser"
	temporaryBlockVariables isCollection
		ifTrue: [ blockNode temporaries: temporaryBlockVariables ]
		ifFalse: [ blockNode temporariesDeclaration: temporaryBlockVariables ].

	self statements: variableNodes innerBlock: true blockNode: blockNode.

	(self match: #rightBracket) ifFalse: [^self expected: 'Period or right bracket'].

	blockNode noteSourceRangeStart: start end: self endOfLastToken encoder: encoder.

	"The scope of the parameters and temporary block variables is no longer active."
	"RNG - temporary change until we fully start using temporary declaration nodes in the parser"
	temporaryBlockVariables isCollection
		ifTrue: [ temporaryBlockVariables do: [:variable | variable scope: -1] ]
		ifFalse: [ temporaryBlockVariables allDeclaredVariableNodes do: [:variable | variable scope: -1] ].

	variableNodes do: [:variable | variable scope: -1]! !

!Parser methodsFor: 'expression types' stamp: 'RNG 3/13/2020 00:16:04'!
method: noPattern doIt: doIt context: ctxt 
	" pattern [ | temporaries ] block => MethodNode."

	| sap block primitives temporaries messageComment methodNode |
	
	sap := self pattern: noPattern inContext: ctxt.
	"sap={selector, arguments, precedence}"
	self properties selector: (sap at: 1).
	encoder selector: (sap at: 1).
	(sap at: 2) do: [:argNode | argNode beMethodArg].
	
	doIt ifFalse: [self pragmaSequence].
	temporaries := self temporaries.
	messageComment := currentComment.
	currentComment := nil.
	doIt ifFalse: [self pragmaSequence].
	primitives := self pragmaPrimitives.
	
	self statements: #() innerBlock: doIt.
	block := parseNode.
	doIt 
		ifTrue: [block returnLast]
		ifFalse: [block returnSelfIfNoOther: encoder].
	hereType == #doIt ifFalse: [^self expected: 'Nothing more'].
	
	methodNode := self newMethodNode comment: messageComment.
	
	"RNG - temporary change, until we start using temporary declaration nodes in the parser"
	temporaries isCollection
		ifTrue: [
			methodNode
				selector: (sap at: 1)
				arguments: (sap at: 2)
				precedence: (sap at: 3)
				temporaries: temporaries
				block: block
				encoder: encoder
				primitive: primitives
				properties: properties
				selectorKeywordsRanges: (sap at: 4) ]
		ifFalse: [
			methodNode
				selector: (sap at: 1)
				arguments: (sap at: 2)
				precedence: (sap at: 3)
				temporariesDeclaration: temporaries
				block: block
				encoder: encoder
				primitive: primitives
				properties: properties
				selectorKeywordsRanges: (sap at: 4) ].
		
	self interactive ifTrue: [ self performInteractiveChecks: methodNode ].
	
	^methodNode! !

!Parser methodsFor: 'expression types' stamp: 'RNG 3/13/2020 00:24:08'!
temporaries
	" [ '|' (variable)* '|' ]"
	| tempDeclarationNodes theActualText declarationStartIndex |
	(self match: #verticalBar) ifFalse: 
		["no temps"
		doitFlag ifTrue:
			[tempsMark := self interactive
								ifTrue: [requestor selectionInterval first]
								ifFalse: [1].
			^ self noTempsDeclaration ].
		tempsMark := hereMark	"formerly --> prevMark + prevToken".
		tempsMark > 0 ifTrue:
			[theActualText := source contents.
			[tempsMark < theActualText size and: [(theActualText at: tempsMark) isSeparator]]
				whileTrue: [tempsMark := tempsMark + 1]].
			^ self noTempsDeclaration ].
	tempDeclarationNodes _ OrderedCollection new.
	declarationStartIndex _ prevMark.
	[hereType == #word] whileTrue: [
		self advanceWithRangeDo: [ :variableName :range |
			| variableNode |
			variableNode _ encoder bindTemp: variableName range: range.
			tempDeclarationNodes addLast: (self createTempDeclarationOf: variableNode sourceRange: range) ] ].
	(self match: #verticalBar) ifTrue: [
		tempsMark := prevMark.
		^ self
			createTempsDeclarationWith: tempDeclarationNodes
			sourceRange: (declarationStartIndex to: prevMark) ].
	^ self expected: 'Vertical bar'! !

!Parser methodsFor: 'expression types' stamp: 'RNG 3/13/2020 00:24:05'!
temporaryBlockVariablesFor: aBlockNode
	"Scan and answer temporary block variables."

	| tempDeclarationNodes declarationStartIndex |
	(self match: #verticalBar) ifFalse:
		"There are't any temporary variables."
		[aBlockNode tempsMark: prevMark + requestorOffset.
		 ^ self noTempsDeclaration ].

	tempDeclarationNodes _ OrderedCollection new.
	declarationStartIndex _ prevMark.
	[hereType == #word] whileTrue: [
		self advanceWithRangeDo: [ :variableName :range |
			| variableNode |
			variableNode _ encoder bindBlockTemp: variableName within: aBlockNode range: range.
			tempDeclarationNodes addLast: (self createTempDeclarationOf: variableNode sourceRange: range) ] ].
	(self match: #verticalBar) ifFalse:
		[^self expected: 'Vertical bar'].
	aBlockNode tempsMark: prevMark + requestorOffset.
	^ self
		createTempsDeclarationWith: tempDeclarationNodes
		sourceRange: (declarationStartIndex to: prevMark)! !


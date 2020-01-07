'From Cuis 5.0 [latest update: #3987] on 23 December 2019 at 8:59:27 am'!

!CodeNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 08:49:29'!
hasLocallyArgumentNamed: aVariableName
		
	^self hasNodeIn: self arguments named: aVariableName! !

!CodeNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 08:49:24'!
hasLocallyTemporaryNamed: aVariableName

	^self hasNodeIn: self temporaries named: aVariableName ! !


!CodeNode methodsFor: 'testing' stamp: 'HAW 12/23/2019 08:47:40'!
hasLocallyArgumentOrTemporaryNamed: aVariableName

	"- hasArgumentOrTemporaryNamed: returns true if there is a temp or variable in this scope or subscopes 
	   named aVariableName
	- hasLocalName: returns true if there is a variable in the scopeTable name aVariableName. That includes 
	  temps, arguments, instance variables and pseudo-variables
	- hasLocallyArgumentOrTemporaryNamed: returns true if ony this scope defines a temp or argument 
	  named aVariableName. - Hernan" 
	
	^(self hasLocallyArgumentNamed: aVariableName)
		or: [self hasLocallyTemporaryNamed: aVariableName]! !


!ArgumentDeclarationCounter methodsFor: 'visiting' stamp: 'HAW 12/23/2019 08:48:15'!
declaresSoughtArgument: aParseNode

	^aParseNode hasLocallyArgumentNamed: argumentName 
	
	! !


!ParseNodesDeclaringTemporaryVariableVisitor methodsFor: 'visiting' stamp: 'HAW 12/23/2019 08:51:24'!
isNodeDeclaringTemporary: aParseNode

	^aParseNode hasLocallyTemporaryNamed: temporaryVariable 
	! !

!ParseNodesDeclaringTemporaryVariableVisitor methodsFor: 'visiting' stamp: 'HAW 12/23/2019 08:53:50'!
visitBlockNode: aBlockNode

	(self isNodeDeclaringTemporary: aBlockNode) ifTrue: [ 
		self visitBlockNodeDeclaringTemporary: aBlockNode ].
	
	super visitBlockNode: aBlockNode.! !

!ParseNodesDeclaringTemporaryVariableVisitor methodsFor: 'visiting' stamp: 'HAW 12/23/2019 08:53:57'!
visitMethodNode: aMethodNode

	(self isNodeDeclaringTemporary: aMethodNode) ifTrue: [
		self visitMethodNodeDeclaringTemporary: aMethodNode ].
	
	super visitMethodNode: aMethodNode.! !


!TemporaryVariableDeclarationRemover methodsFor: 'visiting' stamp: 'HAW 12/23/2019 08:54:15'!
visitNodeDeclaringTemporary: aParseNode 
	withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock

	newSourceCode _ self
		sourceTextWithoutTemporaryFromParseNode: aParseNode
		withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock.
		
	methodNode methodClass compile: newSourceCode.! !


!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'HAW 12/23/2019 08:52:40'!
assert: aMethodNode hasTemporaryNamed: aTemporaryVariableName

	| counter |

	counter := TemporaryVariableDeclarationCounter for: aTemporaryVariableName.
	aMethodNode accept: counter.
	
	counter count = 1 ifFalse: [
		self refactoringError: self inexistentTemporaryErrorDescription ].! !


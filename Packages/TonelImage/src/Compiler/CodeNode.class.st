Class {
	#name : 'CodeNode',
	#superclass : 'ParseNode',
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'accessing' }
CodeNode >> arguments [

	self subclassResponsibility 
]

{ #category : 'accessing' }
CodeNode >> arguments: aCollectionOfArguments [

	self subclassResponsibility 
]

{ #category : 'accessing' }
CodeNode >> block [

	self subclassResponsibility 
]

{ #category : 'printing' }
CodeNode >> decompileString [

	self subclassResponsibility 
]

{ #category : 'private' }
CodeNode >> hasEquivalentArgumentsWith: aCodeNode [

	^self areNodesOf: self arguments equivalentTo: aCodeNode arguments
]

{ #category : 'private' }
CodeNode >> hasEquivalentTemporariesDeclarationWith: aCodeNode [

	(self temporariesDeclaration isNil and: [ aCodeNode temporariesDeclaration isNil ]) ifTrue: [ ^true ].
	(self temporariesDeclaration isNil and: [ aCodeNode temporariesDeclaration notNil ]) ifTrue: [ ^false ].
	(self temporariesDeclaration notNil and: [ aCodeNode temporariesDeclaration isNil ]) ifTrue: [ ^false ].
	
	^self temporariesDeclaration equivalentTo: aCodeNode temporariesDeclaration 
]

{ #category : 'testing' }
CodeNode >> hasLocallyArgumentNamed: aVariableName [
		
	^self hasNodeIn: self arguments named: aVariableName
]

{ #category : 'testing' }
CodeNode >> hasLocallyArgumentOrTemporaryNamed: aVariableName [

	"- hasArgumentOrTemporaryNamed: returns true if there is a temp or variable in this scope or subscopes 
	   named aVariableName
	- hasLocalName: returns true if there is a variable in the scopeTable name aVariableName. That includes 
	  temps, arguments, instance variables and pseudo-variables
	- hasLocallyArgumentOrTemporaryNamed: returns true if ony this scope defines a temp or argument 
	  named aVariableName. - Hernan" 
	
	^(self hasLocallyArgumentNamed: aVariableName)
		or: [self hasLocallyTemporaryNamed: aVariableName]
]

{ #category : 'testing' }
CodeNode >> hasLocallyTemporaryNamed: aVariableName [

	^self hasNodeIn: self temporaries named: aVariableName 
]

{ #category : 'testing' }
CodeNode >> hasNodeIn: aCollectionOfNodes named: aName [

	^aCollectionOfNodes anySatisfy: [ :tempNode | tempNode isNamed: aName ]
]

{ #category : 'testing' }
CodeNode >> hasTemporaryVariables [

	^ self temporariesDeclaration declaresAnyVariable
]

{ #category : 'testing' }
CodeNode >> isLocalArgumentOrTemporary: aParseNode [

	"Looks only in this scope - Hernan"
	^(self temporaries includes: aParseNode) or: [ self arguments includes: aParseNode ]
	
]

{ #category : 'accessing' }
CodeNode >> temporaries [

	self subclassResponsibility 
]

{ #category : 'accessing' }
CodeNode >> temporaries: aCollectionOfTemporaries [

	self subclassResponsibility 
]

{ #category : 'accessing' }
CodeNode >> temporariesDeclaration [

	self subclassResponsibility
]

{ #category : 'accessing' }
CodeNode >> temporariesDeclaration: aTemporariesDeclarationNode [

	self subclassResponsibility
]

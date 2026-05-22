Class {
	#name : 'ParseNodeEnumeratorTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Compiler',
	#package : 'BaseImageTests',
	#tag : 'Compiler'
}

{ #category : 'tests about enumeration' }
ParseNodeEnumeratorTest >> testItEnumeratesAllTheParseNodesPresentInAMethodNode [

	| blockNode enumeratedNodes methodNode enumerator tempVarsDeclarationNode tempVarDeclarationNode literalNode messageNode nodesToBeEnumerated returnNode variableNode |
	methodNode := [ Parser parse: 'm1 | arg | ^ arg + 2' class: self class ]
		on: SyntaxErrorNotification 
		do: [ :anError | self fail ].

	enumeratedNodes := Set new.
	enumerator := ParseNodeEnumerator ofBlock: [ :parseNode | enumeratedNodes add: parseNode ].
	methodNode accept: enumerator.
	blockNode := methodNode block.
	tempVarsDeclarationNode := methodNode temporariesDeclaration.
	tempVarDeclarationNode := tempVarsDeclarationNode temporaryDeclarationNodes first.
	returnNode := blockNode statements first.
	messageNode := returnNode expr.
	variableNode := messageNode receiver.
	literalNode := messageNode arguments first.
	nodesToBeEnumerated := {
		methodNode . blockNode . tempVarsDeclarationNode . tempVarDeclarationNode.
		returnNode . messageNode . variableNode . literalNode.
	}.

	self assert: (enumeratedNodes includesAllOf: nodesToBeEnumerated)
]

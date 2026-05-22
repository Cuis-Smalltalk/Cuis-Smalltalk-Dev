"
A VariableScopeFinder is used to find the minimum enclosing scope of a variable in a method.  This is used when auto-declaring temporaries to find the smallest enclosing block in which to declare the temp.

Instance Variables
	theVariable:		<VariableNode>

theVariable
	- the varable whose scope is to be determined

"
Class {
	#name : 'VariableScopeFinder',
	#superclass : 'ParseNodeVisitor',
	#instVars : [
		'theVariable'
	],
	#category : 'Compiler-Support',
	#package : 'Compiler',
	#tag : 'Support'
}

{ #category : 'private' }
VariableScopeFinder >> enclosingNodeFor: enumerator of: rootNode [
	"Answer the minimum enclosing root node for aVariabe or nil if none.
	 If the variable is accessed in more than one subnode then the rootNode is the
	 enclosing node, otherwise it is which ever single subnode node that includes it, if any.
	 enumerator applies its argument to all relevant subnodes of rootNode."
	| enclosingNodeOrNil |
	enclosingNodeOrNil := nil.
	enumerator value:
		[:subnode|
		(subnode accept: self) ifNotNil:
			[:enclosingNode|
			enclosingNodeOrNil := enclosingNodeOrNil
										ifNil: [enclosingNode]
										ifNotNil: [rootNode]]].
	^enclosingNodeOrNil
]

{ #category : 'initialization' }
VariableScopeFinder >> ofVariable: aVariableNode [
	theVariable := aVariableNode
]

{ #category : 'visiting' }
VariableScopeFinder >> visitAssignmentNode: anAssignmentNode [
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then anAssignmentNode
	 is the enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock|
							aBlock
								value: anAssignmentNode value;
								value: anAssignmentNode variable]
		of: anAssignmentNode
]

{ #category : 'visiting' }
VariableScopeFinder >> visitBlockNode: aBlockNode [
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one statement then aBlockNode is the
	 enclosing node, otherwise it is which ever single block node that includes it, if any."
	^(self enclosingNodeFor: [:aBlock| aBlockNode statementsDo: aBlock] of: aBlockNode) ifNotNil:
		[:aNode|
		aNode isBlockNode ifTrue: [aNode] ifFalse: [aBlockNode]]
]

{ #category : 'visiting' }
VariableScopeFinder >> visitBraceNode: aBraceNode [
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then aBraceNode
	 is the enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock| aBraceNode elementsDo: aBlock]
		of: aBraceNode
]

{ #category : 'visiting' }
VariableScopeFinder >> visitCascadeNode: aCascadeNode [
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then aMessageNode is the
	 enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock|
							aBlock value: aCascadeNode receiver.
							aCascadeNode messages do:
								[:msg| msg argumentsInEvaluationOrder do: aBlock]]
		of: aCascadeNode
]

{ #category : 'visiting' }
VariableScopeFinder >> visitInstanceVariableNode: aNode [
	^nil
]

{ #category : 'visiting' }
VariableScopeFinder >> visitLiteralNode: aNode [
	^nil
]

{ #category : 'visiting' }
VariableScopeFinder >> visitLiteralVariableNode: aNode [
	^nil
]

{ #category : 'visiting' }
VariableScopeFinder >> visitMessageNode: aMessageNode [
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then aMessageNode is the
	 enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock|
							aBlock value: aMessageNode receiver.
							aMessageNode argumentsInEvaluationOrder do: aBlock]
		of: aMessageNode
]

{ #category : 'visiting' }
VariableScopeFinder >> visitMethodNode: aMethodNode [
	^aMethodNode block accept: self
]

{ #category : 'visiting' }
VariableScopeFinder >> visitNewArrayNode: aNode [
	^nil
]

{ #category : 'visiting' }
VariableScopeFinder >> visitRemoteTempVectorNode: aNode [
	^nil
]

{ #category : 'visiting' }
VariableScopeFinder >> visitReturnNode: aReturnNode [
	^aReturnNode expr accept: self
]

{ #category : 'visiting' }
VariableScopeFinder >> visitSelectorNode: aNode [
	^nil
]

{ #category : 'visiting' }
VariableScopeFinder >> visitTempVariableNode: aNode [
	^nil
]

{ #category : 'visiting' }
VariableScopeFinder >> visitUndeclaredVariableNode: aVariableNode [
	^theVariable name = aVariableNode name ifTrue: [theVariable]
]

{ #category : 'visiting' }
VariableScopeFinder >> visitVariableNode: aVariableNode [
	^nil
]

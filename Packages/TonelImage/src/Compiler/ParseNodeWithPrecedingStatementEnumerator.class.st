"
A ParseNodeWithPrecedingStatementEnumerator is a ParseNodeEnumerator that accepts a binary block in ofBlock:, and hence enumerates statement nodes with their preceding statement, or nil if they are the first.

Instance Variables
	precedingStatement:		<ParseNode | nil>

precedingStatement
	- the preceding statement node, if any

"
Class {
	#name : 'ParseNodeWithPrecedingStatementEnumerator',
	#superclass : 'ParseNodeEnumerator',
	#instVars : [
		'precedingStatement'
	],
	#category : 'Compiler-Support',
	#package : 'Compiler',
	#tag : 'Support'
}

{ #category : 'initialize-release' }
ParseNodeWithPrecedingStatementEnumerator >> ofBlock: aBlock [
	"N.B. This enumerator visits a node before any of the node's children.
	 Hence, when enumerating statements in a block, we can ensure that
	 the second argument to the block, the preceding statement, is non-nil
	 only for top-level statements in the block by nilling out precedingStatement
	 once the block is evaluated. Perhaps stronger would be to capture its value
	 in a temporary and nil it before evaluating, but this is good enough."
	theBlock := [:node|
				aBlock value: node value: precedingStatement.
				precedingStatement := nil]
]

{ #category : 'initialize-release' }
ParseNodeWithPrecedingStatementEnumerator >> ofBlock: aBlock select: aSelectBlock [
	self ofBlock: aBlock.
	theSelectBlock := aSelectBlock
]

{ #category : 'visiting' }
ParseNodeWithPrecedingStatementEnumerator >> visitBlockNode: aBlockNode [
	| savedPrecedingStatement |
	(theSelectBlock isNil or: [theSelectBlock value: aBlockNode]) ifFalse:
		[^nil].
	theBlock value: aBlockNode.
	savedPrecedingStatement := precedingStatement.
	precedingStatement := nil.
	[aBlockNode statements do:
		[:statement|
		 statement accept: self.
		 precedingStatement := statement]] ensure:
		[precedingStatement := savedPrecedingStatement]
]

"
I find the BlockNode parents of a BlockNode in the ParseNode tree starting from a ParseNode supplied to #parentsIn:. 
"
Class {
	#name : 'BlockNodeParentsFinder',
	#superclass : 'ParseNodeVisitor',
	#instVars : [
		'selectedBlockNode',
		'root',
		'parents',
		'found'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
BlockNodeParentsFinder class >> for: aSelectedBlockNode [

	^self new initializeFor: aSelectedBlockNode.
]

{ #category : 'initializing' }
BlockNodeParentsFinder >> initializeFor: aSelectedBlockNode [

	selectedBlockNode := aSelectedBlockNode.
	parents := OrderedCollection new.
	found := false.
]

{ #category : 'accessing' }
BlockNodeParentsFinder >> parentsIn: aParseNode [

	aParseNode accept: self.
	^parents
]

{ #category : 'visiting' }
BlockNodeParentsFinder >> visitBlockNode: aBlockNode [

	found ifFalse: [
		aBlockNode = selectedBlockNode
			ifTrue: [ found := true ]
			ifFalse: [
				parents add: aBlockNode.
				super visitBlockNode: aBlockNode.
				found ifFalse: [ parents removeLast ]
			]
	]


	
]

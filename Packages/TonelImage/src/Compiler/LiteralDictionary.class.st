"
A LiteralDictionary, like an IdentityDictionary, has a special test for equality.  In this case it is simple equality between objects of like class.  This allows equal Float or String literals to be shared without the possibility of erroneously sharing, say, 1 and 1.0
"
Class {
	#name : 'LiteralDictionary',
	#superclass : 'Dictionary',
	#category : 'Compiler-Support',
	#package : 'Compiler',
	#tag : 'Support'
}

{ #category : 'as yet unclassified' }
LiteralDictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or raise an error if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start |
	index := start := anObject hash \\ array size + 1.
	[ 
		| element |
		((element := array at: index) == nil or: [
			element key literalEqual: anObject ])
				ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self error: 'There is no free space in this collection!'
]

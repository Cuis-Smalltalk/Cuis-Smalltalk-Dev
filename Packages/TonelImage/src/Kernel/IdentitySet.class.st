"
The same as a Set, except that items are compared using #== instead of #=.

Almost any class named IdentityFoo is the same as Foo except for the way items are compared.  In Foo, #= is used, while in IdentityFoo, #== is used.  That is, identity collections will treat items as the same only if they have the same identity.

For example, note that copies of a string are equal:

	('abc' copy) = ('abc' copy)

but they are not identitcal:

	('abc' copy) == ('abc' copy)

A regular Set will only include equal objects once:

	| aSet |
	aSet := Set new.
	aSet add: 'abc' copy.
	aSet add: 'abc' copy.
	aSet


An IdentitySet will include multiple equal objects if they are not identical:

	| aSet |
	aSet := IdentitySet new.
	aSet add: 'abc' copy.
	aSet add: 'abc' copy.
	aSet

"
Class {
	#name : 'IdentitySet',
	#superclass : 'Set',
	#category : 'Kernel-Collections',
	#package : 'Kernel',
	#tag : 'Collections'
}

{ #category : 'accessing' }
IdentitySet >> elementForIdentityHash: aNumber [
	"Answer any element matching argument.
	Answer nil if none found"
	| finish scale index element |
	finish := array size.
	scale := finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale := 1].
	index := aNumber * scale \\ finish + 1.

	element := array at: index.
	^element identityHash = aNumber ifTrue: [ element ]
]

{ #category : 'private' }
IdentitySet >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish scale start element |
	finish := array size.
	scale := finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale := 1].
	start := anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]

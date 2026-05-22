"
See class comment at WeakSet.

This subclass only differs from it in using #== and #identityHash instead of #= and #hash.
"
Class {
	#name : 'WeakIdentitySet',
	#superclass : 'WeakSet',
	#category : 'Collections-Unordered',
	#package : 'Collections',
	#tag : 'Unordered'
}

{ #category : 'private-equality or identity' }
WeakIdentitySet >> areEqual: a and: b [
	"To make it easy to implement WeakIdentitySet as a subclass."

	^a == b
]

{ #category : 'private-equality or identity' }
WeakIdentitySet >> basicLook: anObject inIterablePartOf: aBucket [
	"To make it easy to implement WeakIdentitySet as a subclass."
	"Answer the index or nil if not found"

	self bucketIterablePartStart to: aBucket size do: [ :i |
		(aBucket at: i) == anObject ifTrue: [ ^i ]].
	^nil
]

{ #category : 'private-equality or identity' }
WeakIdentitySet >> hashFor: anObject [
	"To make it easy to implement WeakIdentitySet as a subclass."

	^anObject identityHash
]

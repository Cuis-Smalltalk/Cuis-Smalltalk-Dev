"
My instances are very much like Dictionaries.
The only difference is that they remember the order in which keys were added, and iterators like #do:, #keysDo: and #associationsDo: will do the iteration in that deterministic order.
"
Class {
	#name : 'OrderedDictionary',
	#superclass : 'Dictionary',
	#instVars : [
		'orderedKeys'
	],
	#category : 'Collections-Sequenceable',
	#package : 'Collections',
	#tag : 'Sequenceable'
}

{ #category : 'enumerating' }
OrderedDictionary >> associationsDo: aBlock  [
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations)."

	orderedKeys do: [ :key |
		aBlock value: (self associationAt: key ifAbsent: nil) ]
]

{ #category : 'accessing' }
OrderedDictionary >> at: newKey replacingKey: oldKey put: anObject [
	"Mostly similar to doing
		aDict removeKey: oldKey; at: newKey put: anObject
	But newKey will be stored in the position (wrt do: and keysDo:) where oldKey was.
	"

	| keyIndex |
	newKey = oldKey ifTrue: [
		^self at: newKey put: anObject ].

	super removeKey: oldKey ifAbsent: [ self errorKeyNotFound: oldKey ].
	keyIndex := orderedKeys indexOf: oldKey.

	(self includesKey: newKey) ifTrue: [
		"Do expensive linear seach only if newKey was already there, so we can't tell where it was."
		orderedKeys remove: newKey ].
	self at: newKey put: anObject.
	orderedKeys last = newKey ifTrue: [
		orderedKeys removeLast ].
	orderedKeys at: keyIndex put: newKey.

	^anObject
]

{ #category : 'accessing' }
OrderedDictionary >> atLast: key put: anObject [
	"Similar to #at:put:, but the key will be at the end, even if it was already there."
	self at: key put: anObject.
	orderedKeys last = key ifFalse: [
		orderedKeys remove: key; addLast: key ].
	^anObject
]

{ #category : 'private' }
OrderedDictionary >> atNewIndex: index put: anAssociation [

	super atNewIndex: index put: anAssociation.
	orderedKeys add: anAssociation key
]

{ #category : 'enumerating' }
OrderedDictionary >> do: aBlock [
	"Evaluate aBlock for each of the receiver's values."

	orderedKeys do: [ :key |
		aBlock value: (self at: key ifAbsent: nil) ]
]

{ #category : 'inspecting' }
OrderedDictionary >> explorerContentsWithIndexCollect: twoArgBlock [

	^ orderedKeys collect: [ :k |
		twoArgBlock value: (self at: k) value: k ].
]

{ #category : 'private' }
OrderedDictionary >> init: n [

	super init: n.
	orderedKeys := OrderedCollection new
]

{ #category : 'enumerating' }
OrderedDictionary >> keysDo: aBlock  [
	"Evaluate aBlock for each of the receiver's keys."

	orderedKeys do: aBlock 
]

{ #category : 'accessing' }
OrderedDictionary >> keysSortedSafely [
	"Answer a sorted Collection containing the receiver's keys.
	Redefined from Dictionary: for us, propery sorted keys are keys in the order they were added."
	^ orderedKeys
]

{ #category : 'private' }
OrderedDictionary >> postCopy [
	"Must copy the orderedKeys as well"

	super postCopy.
	orderedKeys := orderedKeys copy
]

{ #category : 'removing' }
OrderedDictionary >> removeKey: key ifAbsent: aBlock  [
	"Remove key (and its associated value) from the receiver. If key is not in
	the receiver, answer the result of evaluating aBlock. Otherwise, answer
	the value externally named by key."

	| answer |
	answer := super removeKey: key ifAbsent: [
		^ aBlock value ].
	orderedKeys remove: key.
	^ answer
]

{ #category : 'enumerating' }
OrderedDictionary >> reverseDo: aBlock [
	"Evaluate aBlock for each of the receiver's values in reverse order."

	orderedKeys reverseDo: [ :key |
		aBlock value: (self at: key ifAbsent: nil) ]
]

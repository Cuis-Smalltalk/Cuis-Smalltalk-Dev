"
A replacement for RunArray that does not optimize space. Essentially just an array, with a few idiosyncratic methods for compatibility with RunArray.

Rationale: When styling Smalltalk text, runs are very short. Space saving is not significant. Maybe 90% of the time is spent building and coalescing RunArrays. So, don't save space. Save time!
"
Class {
	#name : 'RunNotArray',
	#superclass : 'ArrayedCollection',
	#category : 'Collections-Arrayed',
	#package : 'Collections',
	#tag : 'Arrayed'
}

{ #category : 'accessing' }
RunNotArray >> at: index [
	"Tolerates index=0 and index=size+1 for copyReplace: compatibility.
	This is intentional: copyReplace:to:with: computes boundary indices (start-1 and stop+1)
	that fall outside the valid range; these sentinel accesses must not raise errors."
	index = 0 ifTrue: [ ^ self at: 1 ].
	index = (self size+1) ifTrue: [ ^ self at: index-1 ].
	^ super at: index
]

{ #category : 'Text support' }
RunNotArray >> basicReplaceAttributesFrom: start to: stop with: replacement [

	self replaceFrom: start to: stop with: replacement startingAt: 1 
]

{ #category : 'accessing' }
RunNotArray >> canJoinMessage: aMessageSend [
	"Nop. We never try to join."
]

{ #category : 'adding' }
RunNotArray >> coalesce [
	"Nop. We never combine adjacent runs"
]

{ #category : 'Text support' }
RunNotArray >> find: attribute [
	"Return the first interval over which this attribute applies"

	1 to: self size do: [ :i |
		((self at: i) includes: attribute) ifTrue: [
			^ self rangeOf: attribute startingAt: i ]].
	
	^ nil
]

{ #category : 'Text support' }
RunNotArray >> mapValues: mapBlock [
	self withIndexDo: [ :each :i |
		self at: i put: (mapBlock value: each) ]
]

{ #category : 'accessing' }
RunNotArray >> rangeOf: attribute startingAt: startPos [
	"Answer an interval that gives the range of attr at index position  startPos. An empty interval with start value startPos is returned when the attribute attr is not present at position startPos.  self size > 0 is assumed, it is the responsibility of the caller to test for emptiness of self."
	
	| firstRelevantPosition lastRelevantPosition value |
	value := self at: startPos.
	(value includes: attribute) ifFalse: [
		^ startPos to: startPos - 1].

	lastRelevantPosition := startPos +1.
	[lastRelevantPosition <= self size 
		and: [(self at: lastRelevantPosition) includes: attribute]]
	whileTrue: [
		lastRelevantPosition := lastRelevantPosition + 1 ].

	firstRelevantPosition := startPos -1.
	[firstRelevantPosition >= 1 
		and: [(self at: firstRelevantPosition) includes: attribute]]
	whileTrue: [
		firstRelevantPosition := firstRelevantPosition - 1]. 
 
	^firstRelevantPosition+1 to: lastRelevantPosition-1
]

{ #category : 'private' }
RunNotArray >> replaceFrom: start to: stop with: replacement startingAt: repStart  [
	"Copied from Array"
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]

{ #category : 'accessing' }
RunNotArray >> runLengthAt: i0 [
	"
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 1.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 2.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 3.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 4.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 5.
	"

	| value |
	value := self at: i0.
	i0+1 to: self size do: [ :i |
		(self at: i) = value ifFalse: [ ^ i-i0 ]].
	^ self size-i0+1
		
]

{ #category : 'enumerating' }
RunNotArray >> runsAndValuesDo: aBlock [
	"Evaluate aBlock with run lengths and values from the receiver"
	self do: [ :attributes |
		aBlock value: 1 value: attributes ]
]

{ #category : 'enumerating' }
RunNotArray >> runsFrom: start to: stop do: aBlock [
	start to: stop do: [ :i | aBlock value: (self at: i )]
]

{ #category : 'Text support' }
RunNotArray >> values [
	"yep. self"
]

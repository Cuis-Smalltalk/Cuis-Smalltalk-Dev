"
I am an efficient collection of Strings. I behave a bit like a Dictionary, with the restriction that keys are instances of String.
Notes:
	- check for inclusion is extremely fast
	- iteration is always done in collation order, contents are always sorted without performance cost
	- Behaves both like a Set (#add:, #remove:, #includes, #do) and a Dictionary (#at:put:, #at:, #at:ifAbsent:)
	
| t |
t := Trie new.
t add: 'car'.
t at: 'car' put: Float pi.
t at: 'cat' put: Date today.
t explore.
(t includesKey: 'car') print.
(t includes: 'cat') print

| t |
t := Trie new.
Smalltalk allImplementedMessages do: [ :s | t add: s ].
t explore
"
Class {
	#name : 'Trie',
	#superclass : 'Collection',
	#instVars : [
		'rootNode'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'adding' }
Trie >> add: aString [

	self at: aString put: aString
]

{ #category : 'accessing' }
Trie >> at: key  [
	"Answer the value associated with the key."

	^ self at: key ifAbsent: [ self errorKeyNotFound: key ]
]

{ #category : 'accessing' }
Trie >> at: aString ifAbsent: aBlock [

	rootNode ifNil: [ ^aBlock value ].
	^ rootNode at: aString ifAbsent: aBlock characterIndex: 1
]

{ #category : 'accessing' }
Trie >> at: key ifAbsentPut: aBlock  [
	"Return the value at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."
	^self at: key ifAbsent:[self at: key put: aBlock value]
]

{ #category : 'accessing' }
Trie >> at: key ifPresent: aBlock [
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [^ nil].
	^ aBlock value: v
]

{ #category : 'accessing' }
Trie >> at: aString put: aValue [

	rootNode ifNil: [
		rootNode := TrieNode someKey: aString segmentStart: 1].
	^rootNode at: aString put: aValue characterIndex: 1
]

{ #category : 'enumerating' }
Trie >> do: aBlock [
	"Consistent both with Set (#add:) and Dictionary (#at:put:) protocols."
	self valuesDo: aBlock
]

{ #category : 'private' }
Trie >> errorKeyNotFound: key [

	self error: (
		String streamContents: [ :strm |
			strm nextPutAll: 	'key: '.
			key printOn: strm.
			strm nextPutAll: ' not found' ])
]

{ #category : 'enumerating' }
Trie >> forPrefix: aString keysAndValuesDo: twoArgBlock [

	rootNode ifNotNil: [
		rootNode forPrefix: aString keysAndValuesDo: twoArgBlock characterIndex: 1]
]

{ #category : 'testing' }
Trie >> includes: aString [
	"Consistent with Set, but not with Dictionary,  as in Dictionary, #includes:
	finds a value regardless of the key. To get this behavior, use #includesValue:"

	aString isString ifFalse: [ ^ false ].
	self at: aString ifPresent: [ :v | ^v = aString ].
	^false
]

{ #category : 'testing' }
Trie >> includesKey: key [
	
	self at: key ifAbsent: [^false].
	^true
]

{ #category : 'testing' }
Trie >> includesValue: anObject [
	"Similar to Dictionary>>#includes:"
	self do: [:each | anObject = each ifTrue: [^true]].
	^false
]

{ #category : 'testing' }
Trie >> isEmpty [
	^rootNode isNil
]

{ #category : 'enumerating' }
Trie >> keysAndValuesDo: twoArgBlock [

	rootNode ifNotNil: [
		rootNode keysAndValuesDo: twoArgBlock ]
]

{ #category : 'enumerating' }
Trie >> keysDo: aBlock [

	self keysAndValuesDo: [ :k :v | aBlock value: k ]
]

{ #category : 'removing' }
Trie >> remove: aString ifAbsent: exceptionBlock [
	"Consistent with Set"
	self at: aString ifPresent: [ :v |
		v = aString ifTrue: [
			self removeKey: aString ifAbsent: exceptionBlock ]]
]

{ #category : 'removing' }
Trie >> removeKey: aString  [
	"Remove key from the receiver.
	If key is not in the receiver, notify an error."

	^ self removeKey: aString ifAbsent: [ self errorKeyNotFound: aString ]
]

{ #category : 'removing' }
Trie >> removeKey: aString ifAbsent: aBlock [
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	| answer |
	rootNode ifNil: [ ^aBlock value ].
	answer := rootNode removeKey: aString ifAbsent: aBlock characterIndex: 1.
	rootNode isEmpty ifTrue: [
		rootNode := nil ].
	^answer
]

{ #category : 'printing' }
Trie >> storeOn: aStream [
	"Refer to the comment in Object|storeOn:."
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet := true.
	self keysAndValuesDo: 
			[:each :val | 
			noneYet
				ifTrue: [noneYet := false]
				ifFalse: [aStream nextPut: $;].
			aStream 
				nextPutAll: ' at: ';
				store: each;
				nextPutAll: ' put: ';
				store: val].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
]

{ #category : 'enumerating' }
Trie >> valuesDo: aBlock [

	self keysAndValuesDo: [ :k :v | aBlock value: v ]
]

"
My instances form a Trie. They do path compression, meaning that they can represent a segment of seveal characters long.
"
Class {
	#name : 'TrieNode',
	#superclass : 'Object',
	#instVars : [
		'childrenFirstChars',
		'children',
		'segmentSize',
		'someKey'
	],
	#category : 'System-Text',
	#package : 'System',
	#tag : 'Text'
}

{ #category : 'constants' }
TrieNode class >> characterForLeaf [
	"A special character to mean leaf node. Must have lower code than alphabetic chars,
	so in the trie 'car' comes before 'cars'"
	^Character codePoint: 0
]

{ #category : 'instance creation' }
TrieNode class >> someKey: aString segmentStart: i [
	^self new setSegmentSize: aString size - i + 1 someKey: aString
]

{ #category : 'instance creation' }
TrieNode class >> someKey: aString setSegmentSize: s [
	^self new setSegmentSize: s someKey: aString
]

{ #category : 'accessing' }
TrieNode >> at: aString ifAbsent: aBlock characterIndex: i [

	| child |
	child := self childFor: aString characterIndex: i orAdd: false.
	child ifNil: [ ^aBlock value ].
	^child at: aString ifAbsent: aBlock characterIndex: i + segmentSize
]

{ #category : 'accessing' }
TrieNode >> at: aString put: aValue characterIndex: i [

	| child |
	childrenFirstChars ifNotNil: [
		self splitIfNeededFor: aString characterIndex: i ].
	child := self childFor: aString characterIndex: i orAdd: true.
	^child at: aString put: aValue characterIndex: i + segmentSize
]

{ #category : 'private' }
TrieNode >> childFor: aString characterIndex: i orAdd: doAdd [
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil."

	"No children yet"
	childrenFirstChars ifNil: [
		doAdd ifTrue: [
			children := TrieLeaf new.
			childrenFirstChars := TrieNode characterForLeaf ].
		^children ].

	"Just one child, if adding, convert references to a collection"
	childrenFirstChars isCollection ifFalse: [
		^ self childFor1: aString characterIndex: i orAdd: doAdd ].

	"Already more than one child. If adding, convert to a bigger collection."
	^self childForN: aString characterIndex: i orAdd: doAdd
]

{ #category : 'private' }
TrieNode >> childFor1: aString characterIndex: i orAdd: doAdd [
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil.
	This method for the case where we currently have exactly one child."
	| nextSegmentStart nextSegmentFirst answer |

	nextSegmentStart := i + segmentSize.
	nextSegmentFirst := nextSegmentStart > aString size
		ifTrue: [TrieNode characterForLeaf]
		ifFalse: [(aString at: nextSegmentStart) asLowercase asUnaccented].

	childrenFirstChars = nextSegmentFirst ifTrue: [ ^ children].

	doAdd ifFalse: [ ^ nil ].
	
	answer := nextSegmentStart > aString size
		ifTrue: [ TrieLeaf new ]
		ifFalse: [ TrieNode someKey: aString segmentStart: nextSegmentStart ].

	"Just one child, if adding, convert references to a collection"
	nextSegmentFirst < childrenFirstChars
		ifTrue: [
			children := {answer . children }.
			childrenFirstChars := Array with: nextSegmentFirst with: childrenFirstChars ]
		ifFalse: [
			children := {children . answer}.
			childrenFirstChars := Array with: childrenFirstChars with: nextSegmentFirst ].
	^answer
]

{ #category : 'private' }
TrieNode >> childForN: aString characterIndex: i orAdd: doAdd [
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil.
	This method for the case where we currently have more than one child."
	| answer newChildren newFirstChars nextSegmentFirst nextSegmentStart s |

	nextSegmentStart := i + segmentSize.
	nextSegmentFirst := nextSegmentStart > aString size
		ifTrue: [TrieNode characterForLeaf]
		ifFalse: [(aString at: nextSegmentStart) asLowercase asUnaccented].

	childrenFirstChars
		findBinaryIndex: [ :each |
			each = nextSegmentFirst ifTrue: [ 0 ]
				ifFalse: [each > nextSegmentFirst ifTrue: [-1] ifFalse: [1]]]
		do: [ :found | ^ children at: found ]
		ifNone: [ :a :b |
			"Already more than one child. If adding, convert to a bigger collection."
			doAdd
				ifFalse: [ ^nil]
				ifTrue: [
					 answer := nextSegmentStart > aString size
						ifTrue: [ TrieLeaf new ]
						ifFalse: [  TrieNode someKey: aString segmentStart: nextSegmentStart ].
					s := children size + 1.
					newChildren := Array new: s.
					newChildren
						replaceFrom: 1 to: a with: children startingAt: 1;
						at: a+1 put: answer;
						replaceFrom: a+2 to: s with: children startingAt: a+1.
					newFirstChars := Array new: s.
					newFirstChars
						replaceFrom: 1 to: a with: childrenFirstChars startingAt: 1;
						at: a+1 put: nextSegmentFirst;
						replaceFrom: a+2 to: s with: childrenFirstChars startingAt: a+1.
					children := newChildren.
					childrenFirstChars := newFirstChars.
					^answer ]]
]

{ #category : 'enumerating' }
TrieNode >> forPrefix: aString keysAndValuesDo: twoArgBlock characterIndex: i [

	i + segmentSize > aString size
		ifTrue: [
			self keysAndValuesDo: twoArgBlock ]
		ifFalse: [
			(self childFor: aString characterIndex: i orAdd: false)
				ifNotNil: [ :node |
					node
						forPrefix: aString
						keysAndValuesDo: twoArgBlock
						characterIndex: i + segmentSize ]]
]

{ #category : 'private accessors' }
TrieNode >> getChildren [
	"Private. To be called only from parent node"
	^children
]

{ #category : 'private accessors' }
TrieNode >> getChildrenFirstChars [
	"Private. To be called only from parent node"
	^childrenFirstChars
]

{ #category : 'private accessors' }
TrieNode >> getSegmentSize [
	"Private. To be called only from parent node"
	^segmentSize
]

{ #category : 'private accessors' }
TrieNode >> getSomeKey [
	"Private. To be called only from parent node"
	^someKey
]

{ #category : 'testing' }
TrieNode >> isEmpty [
	^childrenFirstChars isNil
]

{ #category : 'enumerating' }
TrieNode >> keysAndValuesDo: twoArgBlock [

	children isArray ifFalse: [
		^children keysAndValuesDo: twoArgBlock ].

	children do: [ :child |
		child ifNotNil: [ child keysAndValuesDo: twoArgBlock ]]
]

{ #category : 'private' }
TrieNode >> removeChildFor: aString characterIndex: i [
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil."
	| nextSegmentStart nextSegmentFirst |

	nextSegmentStart := i + segmentSize.
	nextSegmentFirst := nextSegmentStart > aString size
		ifTrue: [TrieNode characterForLeaf]
		ifFalse: [(aString at: nextSegmentStart) asLowercase asUnaccented].

	"Just one child, if adding, convert references to a collection"
	childrenFirstChars isCollection ifFalse: [
		childrenFirstChars = nextSegmentFirst
			ifTrue: [ childrenFirstChars := nil. children := nil ].
		^self ].
	
	"Already more than one child."
	childrenFirstChars
		findBinaryIndex: [ :each |
			each = nextSegmentFirst ifTrue: [ 0 ]
				ifFalse: [each > nextSegmentFirst ifTrue: [-1] ifFalse: [1]]]
		do: [ :found |
			childrenFirstChars size = 2
				ifTrue: [		"Two children: remove the arrays"
					childrenFirstChars := childrenFirstChars at: 3-found.
					children := children at: 3-found ]
				ifFalse: [	"More than two children. condense the arrays"
					childrenFirstChars := (childrenFirstChars copyFrom: 1 to: found-1), 
						(childrenFirstChars copyFrom: found+1 to: childrenFirstChars size).
					children := (children copyFrom: 1 to: found-1), (children copyFrom: found+1 to: children size) ]]
		ifNone: nil.
]

{ #category : 'removing' }
TrieNode >> removeKey: aString ifAbsent: aBlock characterIndex: i [

	| child answer |
	child := self childFor: aString characterIndex: i orAdd: false.
	child ifNil: [ ^aBlock value ].
	answer := child removeKey: aString ifAbsent: aBlock characterIndex: i + segmentSize.
	child isEmpty ifTrue: [
		self removeChildFor: aString characterIndex: i.
		"If child is not needed anymore, compress the path"	
		children class == TrieNode ifTrue: [
			someKey := children getSomeKey.
			segmentSize := segmentSize + children getSegmentSize.
			childrenFirstChars := children getChildrenFirstChars.
			children := children getChildren ]].
	^answer
]

{ #category : 'private accessors' }
TrieNode >> setChildrenFirstChars: aString children: anArray [
	"Private. To be called only from parent node"
	childrenFirstChars := aString.
	children := anArray.
]

{ #category : 'private accessors' }
TrieNode >> setSegmentSize: anInteger someKey: aString [
	"Private. To be called only from parent node"
	segmentSize := anInteger.
	someKey := aString
]

{ #category : 'private' }
TrieNode >> splitIfNeededFor: aString characterIndex: segmentStart [
	"This method splits the self (the current node) into two nodes, such that the concatenation of their string segment equals our current string segment,
	and such that the string segment of the first node (i.e. us) equals or is a prefix of aString.
	If this latter condition is already met, do nothing."
	| oldSegmentStop newSegmentStop newNode |

	"Compute last index where both strings are equal"
	oldSegmentStop := segmentStart + segmentSize - 1.
	newSegmentStop := someKey commonPartWith: aString startAt: segmentStart stopAt: oldSegmentStop
		applying: [ :c | c asLowercase asUnaccented ].
			
	newSegmentStop = oldSegmentStop ifTrue: [
		"No need to split: our segment is already a prefix of aString"
		^self ].

	"Split node"
	newNode := TrieNode someKey: someKey setSegmentSize: oldSegmentStop - newSegmentStop.
	newNode setChildrenFirstChars: childrenFirstChars children: children.

	"As this is a split point, these will be turned into arrays soon, when the new node addition (that called us) is done."
	childrenFirstChars := (someKey at: newSegmentStop+1) asLowercase asUnaccented.
	children := newNode.
	"Update our segment"
	segmentSize := newSegmentStop - segmentStart + 1
]

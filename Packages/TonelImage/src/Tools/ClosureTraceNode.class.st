"
See comments in ClosureScanner.
"
Class {
	#name : 'ClosureTraceNode',
	#superclass : 'Object',
	#instVars : [
		'parent',
		'object',
		'classOfObject',
		'paths'
	],
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'instance creation' }
ClosureTraceNode class >> new [
	"Copied here to avoid megamorphic message sends.
	Also, these objects do not need to be sent initialize"

	<primitive: 70>
	OutOfMemory signal.
	^self new
]

{ #category : 'instance creation' }
ClosureTraceNode class >> object: anObject [

	^self new
		object: anObject;
		yourself
]

{ #category : 'closure' }
ClosureTraceNode >> breadthFirstCloseOver: anObject for: aFinder [

	1 to: self classOfObject instSize do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]
]

{ #category : 'accessing' }
ClosureTraceNode >> classOfObject [

	^classOfObject
]

{ #category : 'private - accessing' }
ClosureTraceNode >> classOfObject: anObject [

	classOfObject := anObject
]

{ #category : 'private - paths' }
ClosureTraceNode >> cleanUpAfterScan [

	self paths: self paths asArray.
	self paths sort: [:x :y | self path: x comesBefore: y].
	self paths do: [:each | each cleanUpAfterScan]
]

{ #category : 'closure' }
ClosureTraceNode >> depthFirstCloseOver: anObject for: aFinder [

	1 to: self classOfObject instSize do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]
]

{ #category : 'printing' }
ClosureTraceNode >> dumpOn: aStream [

	self dumpOn: aStream depth: 0
]

{ #category : 'printing' }
ClosureTraceNode >> dumpOn: aStream depth: anInteger [

	anInteger timesRepeat: [aStream tab].
	self printOn: aStream.
	aStream newLine.
	self paths do: [:each | each dumpOn: aStream depth: anInteger + 1]
]

{ #category : 'printing' }
ClosureTraceNode >> dumpString [

	^String streamContents: [ :strm |
		self dumpOn: strm ].
]

{ #category : 'private - paths' }
ClosureTraceNode >> foundTargetAtReferent: aReferent [

	| node |
	node := self class object: aReferent.
	node parent: self.
	node recordPath
]

{ #category : 'accessing' }
ClosureTraceNode >> object [

	^object
]

{ #category : 'private - accessing' }
ClosureTraceNode >> object: anObject [

	object := anObject
]

{ #category : 'accessing' }
ClosureTraceNode >> parent [

	^parent
]

{ #category : 'private - accessing' }
ClosureTraceNode >> parent: aTraceNode [

	parent := aTraceNode
]

{ #category : 'private - paths' }
ClosureTraceNode >> path: x comesBefore: y [

	x object identityHash < y object identityHash ifTrue: [^true].
	x object identityHash > y object identityHash ifTrue: [^false].
	^(ReferenceFinderMirror objectClass: x) name
		<= (ReferenceFinderMirror objectClass: y) name
]

{ #category : 'accessing' }
ClosureTraceNode >> paths [

	paths isNil ifTrue: [self paths: Set new].
	^paths
]

{ #category : 'private - accessing' }
ClosureTraceNode >> paths: aSet [

	paths := aSet
]

{ #category : 'printing' }
ClosureTraceNode >> printOn: aStream [

	aStream
		nextPutAll: '> ';
		print: self object
]

{ #category : 'paths' }
ClosureTraceNode >> recordPath [

	self parent isNil ifTrue: [^self].
	self parent paths add: self.
	self parent recordPath
]

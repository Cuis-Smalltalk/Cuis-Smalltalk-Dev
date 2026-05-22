"
See comments in ClosureScanner.
"
Class {
	#name : 'WeightTraceNode',
	#superclass : 'ClosureTraceNode',
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'closure' }
WeightTraceNode >> breadthFirstCloseOver: anObject for: aFinder [
	"Bit objects must be counted, but not traced into"

	1 to: self classOfObject instSize do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	self classOfObject isBits ifTrue: [^self].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]
]

{ #category : 'closure' }
WeightTraceNode >> breadthFirstCloseOver: aTarget into: anObject for: aFinder [

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass := ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node := WeightTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node := WeightTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	self paths add: node.
	aFinder scanQueueAdd: node
]

{ #category : 'closure' }
WeightTraceNode >> depthFirstCloseOver: anObject for: aFinder [
	"Bit objects must be counted, but not traced into"

	1 to: self classOfObject instSize do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	self classOfObject isBits ifTrue: [^self].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]
]

{ #category : 'closure' }
WeightTraceNode >> depthFirstCloseOver: aTarget into: anObject for: aFinder [

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass := ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node := WeightTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node := WeightTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	self paths add: node.
	node depthFirstCloseOver: aTarget for: aFinder
]

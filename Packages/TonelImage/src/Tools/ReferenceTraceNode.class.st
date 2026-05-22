"
See comments in ClosureScanner.
"
Class {
	#name : 'ReferenceTraceNode',
	#superclass : 'ClosureTraceNode',
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'closure' }
ReferenceTraceNode >> breadthFirstCloseOver: aTarget into: anObject for: aFinder [

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass := ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node := ReferenceTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node := ReferenceTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	aFinder scanQueueAdd: node
]

{ #category : 'closure' }
ReferenceTraceNode >> depthFirstCloseOver: aTarget into: anObject for: aFinder [

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass := ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node := ReferenceTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node := ReferenceTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	node depthFirstCloseOver: aTarget for: aFinder
]

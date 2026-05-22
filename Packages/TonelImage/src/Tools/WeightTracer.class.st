"
See comments in ClosureScanner.
"
Class {
	#name : 'WeightTracer',
	#superclass : 'ClosureScanner',
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'private - closure scan storage' }
WeightTracer >> closureTraceCompiledMethodNodeClass [

	^WeightTraceCompiledMethodNode
]

{ #category : 'private - closure scan storage' }
WeightTracer >> closureTraceNodeClass [

	^WeightTraceNode
]

{ #category : 'private - explorer window' }
WeightTracer >> explorerClass [

	^WeightExplorer
]

{ #category : 'private - explorer window' }
WeightTracer >> explorerWindowPrefix [

	^'Objects held solely from '
]

{ #category : 'private - closure scan' }
WeightTracer >> prepareToWeighClosure [

	| mustCleanRootNode |
	mustCleanRootNode := self rootNode notNil.
	self rootNode: (self newRootNodeFor: self target).
	self useDepthFirst ifFalse: [self initializeScanQueue].
	mustCleanRootNode ifFalse: [Smalltalk garbageCollect]
]

{ #category : 'weight' }
WeightTracer >> scanClosureSkipping: anArray [

	self prepareToScanClosure.
	self skipInternalNodesAnd: #().
	self basicScanClosure.
	self prepareToWeighClosure.
	self skipInternalNodesAnd: anArray.
	self basicScanClosure.
	self cleanUpAfterScan
]

{ #category : 'private - closure' }
WeightTracer >> shouldCloseOver: anObject instanceOf: aClass [
	"Bit objects must be counted, but not traced into"

	nil == anObject ifTrue: [^false].
	SmallInteger == aClass ifTrue: [^false].
	(self nodes includes: anObject) ifTrue: [^false].
	self nodes add: anObject.
	^true
]

"
See comments in ClosureScanner.
"
Class {
	#name : 'WeightTraceCompiledMethodNode',
	#superclass : 'WeightTraceNode',
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'closure' }
WeightTraceCompiledMethodNode >> breadthFirstCloseOver: anObject for: aFinder [

	self object literalsDo:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: each
				for: aFinder
		]
]

{ #category : 'closure' }
WeightTraceCompiledMethodNode >> depthFirstCloseOver: anObject for: aFinder [

	self object literalsDo:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: each
				for: aFinder
		]
]

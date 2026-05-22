"
See comments in ClosureScanner.
"
Class {
	#name : 'ReferenceTraceCompiledMethodNode',
	#superclass : 'ReferenceTraceNode',
	#category : 'Tools-ReferenceFinder',
	#package : 'Tools',
	#tag : 'ReferenceFinder'
}

{ #category : 'closure' }
ReferenceTraceCompiledMethodNode >> breadthFirstCloseOver: anObject for: aFinder [

	self object literalsDo:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: each
				for: aFinder
		]
]

{ #category : 'closure' }
ReferenceTraceCompiledMethodNode >> depthFirstCloseOver: anObject for: aFinder [

	self object literalsDo:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: each
				for: aFinder
		]
]

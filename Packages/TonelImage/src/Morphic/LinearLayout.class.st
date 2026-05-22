Class {
	#name : 'LinearLayout',
	#superclass : 'Layout',
	#instVars : [
		'gap',
		'axisEdgeWeight'
	],
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'accessing' }
LinearLayout >> axisEdgeWeight [
	^axisEdgeWeight
]

{ #category : 'accessing' }
LinearLayout >> axisEdgeWeight: aNumberOrNil [
	axisEdgeWeight := aNumberOrNil.
]

{ #category : 'accessing' }
LinearLayout >> gap [
	^gap
]

{ #category : 'accessing' }
LinearLayout >> gap: aNumber [
	gap := aNumber
]

{ #category : 'accessing' }
LinearLayout >> gap: aNumber axisEdgeWeight: aNumberOrNil [
	gap := aNumber.
	axisEdgeWeight := aNumberOrNil.
]

{ #category : 'testing' }
LinearLayout >> isLinearLayout [
	^true
]

{ #category : 'services' }
LinearLayout >> minimumExtentFor: morphs cachedValues: cachedValues [
	self subclassResponsibility 
]

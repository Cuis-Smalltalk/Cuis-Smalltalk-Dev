"
To be used with FormLayout.
Read 'FormLayout examples' in LayoutMorph class.

- 'topLeftEdgesWeight' and 'bottomRightEdgesWeight' are points representing the ox and oy positionning, expressed in percentage in [0;1], of the top left and bottom right corners of the morph, in its FormLayoutMorph owner.
- topLeftOffset and bottomLeftOffset are points representing the ox and oy shifts from the above edges weights, expressed in pixels, postive or negative.

Therefore, the positionning of the morph in its FormLayout owner is determined by its top left and right bottom corners, computed by its 4 edge weights (percentage) in its owner and corrected by its 4 pixel offsets.

"
Class {
	#name : 'LayoutEdgesSpec',
	#superclass : 'LayoutSpec',
	#instVars : [
		'topLeftEdgesWeight',
		'topLeftOffset',
		'bottomRightEdgesWeight',
		'bottomRightOffset'
	],
	#category : 'Morphic-Layouts',
	#package : 'Morphic',
	#tag : 'Layouts'
}

{ #category : 'instance creation' }
LayoutEdgesSpec class >> tlEdgesWeight: tlWeight offset: tlOffset brEdgesWeight: brWeight offset: brOffset [

	^LayoutEdgesSpec new
		topLeftEdgesWeight: tlWeight;
		topLeftOffset: tlOffset;
		bottomRightEdgesWeight: brWeight;
		bottomRightOffset: brOffset
]

{ #category : 'accessing' }
LayoutEdgesSpec >> bottomRightEdgesWeight [
	^bottomRightEdgesWeight
]

{ #category : 'accessing' }
LayoutEdgesSpec >> bottomRightEdgesWeight: aPoint [
	"Argument x and y must be in [0.0 .. 1.0]"
	bottomRightEdgesWeight := aPoint.
	self updateContainingLayout.
]

{ #category : 'accessing' }
LayoutEdgesSpec >> bottomRightOffset [
	^bottomRightOffset
]

{ #category : 'accessing' }
LayoutEdgesSpec >> bottomRightOffset: aPoint [
	bottomRightOffset := aPoint.
	self updateContainingLayout.
]

{ #category : 'testing' }
LayoutEdgesSpec >> isLayoutEdgesSpec [
	^true
]

{ #category : 'layout' }
LayoutEdgesSpec >> minimumSpecExtent [
	"We specify position, but not extent."

	^0@0
]

{ #category : 'accessing' }
LayoutEdgesSpec >> topLeftEdgesWeight [
	^topLeftEdgesWeight
]

{ #category : 'accessing' }
LayoutEdgesSpec >> topLeftEdgesWeight: aPoint [
	"Argument x and y must be in [0.0 .. 1.0]"
	topLeftEdgesWeight := aPoint.
	self updateContainingLayout.
]

{ #category : 'accessing' }
LayoutEdgesSpec >> topLeftOffset [
	^topLeftOffset
]

{ #category : 'accessing' }
LayoutEdgesSpec >> topLeftOffset: aPoint [
	topLeftOffset := aPoint.
	self updateContainingLayout.
]

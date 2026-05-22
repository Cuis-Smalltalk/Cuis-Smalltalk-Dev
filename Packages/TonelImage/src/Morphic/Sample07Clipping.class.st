"
This morph shows clipping to arbitrary shapes. You can get a halo on the star and play with it (move, rotate, resize), and see how clipping works.

See also class comment at Sample01Star.

Evaluate:
Sample07Clipping new openInWorld
"
Class {
	#name : 'Sample07Clipping',
	#superclass : 'PlacedMorph',
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'geometry testing' }
Sample07Clipping >> clipsSubmorphs [

	^ true
]

{ #category : 'drawing' }
Sample07Clipping >> drawOn: aCanvas [

	aCanvas strokeWidth: 15 color: `Color green darker` fillColor: `Color red darker` do: [
		aCanvas
			roundRectangleMinX: -10 maxX: 220 minY: -10 maxY: 120  rx: 120 ry: 50 ]
]

{ #category : 'initialization' }
Sample07Clipping >> initialize [

	super initialize.
	self addMorph: Sample01Star new.
]

"
This morph uses the #coversPixel: and #bitMask features to make it easy to grab although it is made of very thin lines. See #includesPixel: and play with it.
See also class comment at Sample01Star.

Evaluate:
Sample06EasyToGrab new openInWorld
"
Class {
	#name : 'Sample06EasyToGrab',
	#superclass : 'PlacedMorph',
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'drawing' }
Sample06EasyToGrab >> drawOn: aCanvas [

	aCanvas strokeWidth: 1 color: Color black do: [
		aCanvas
			moveTo: 0@0;
			lineTo: 200@200;
			lineTo: -200@200;
			closePath ].
]

{ #category : 'geometry testing' }
Sample06EasyToGrab >> includesPixel: worldPoint [
	"Allow grabbing if close enough.
	See other implementors.
	This is an example of the use of #coversAnyPixelCloserThan:to:
	Usually, you'd prefer something like (better if super call is actually inlined):
	-10 to: 10 do: [ :dy |
		-10 to: 10 do: [ :dx |
			(super includesPixel: worldPoint + (dx@dy)) ifTrue: [ ^true ]]].
	^false
	"

	^ self visible and: [self coversAnyPixelCloserThan: 10 to: worldPoint].
]

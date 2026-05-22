"
Sample09Clock new openInWorld
"
Class {
	#name : 'Sample09ClockMinuteHand',
	#superclass : 'PlacedMorph',
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'drawing' }
Sample09ClockMinuteHand >> drawOn: aCanvas [

	aCanvas fillColor: `Color darkGray` do: [
		aCanvas
			moveTo: `0 @ 8`;
			lineTo: `-4 @ 0`;
			lineTo: `0 @ -82`;
			lineTo: `4 @ 0`;
			lineTo: `0 @ 8` ].
]

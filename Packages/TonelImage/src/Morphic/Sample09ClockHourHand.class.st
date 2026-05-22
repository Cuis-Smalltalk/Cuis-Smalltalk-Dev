"
Sample09Clock new openInWorld
"
Class {
	#name : 'Sample09ClockHourHand',
	#superclass : 'PlacedMorph',
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'drawing' }
Sample09ClockHourHand >> drawOn: aCanvas [

	aCanvas fillColor: `Color darkGray` do: [
		aCanvas
			moveTo: `0 @ 10`;
			lineTo: `-5 @ 0`;
			lineTo: `0 @ -50`;
			lineTo: `5 @ 0`;
			lineTo: `0 @ 10` ].
]

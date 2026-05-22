"
Sample09Clock new openInWorld
"
Class {
	#name : 'Sample09ClockSecondHand',
	#superclass : 'PlacedMorph',
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'drawing' }
Sample09ClockSecondHand >> drawOn: aCanvas [

	aCanvas strokeWidth: 3.5 color: Color red do: [
		aCanvas
			moveTo: `0 @ 0`;
			lineTo: `0 @ -85` ].
]

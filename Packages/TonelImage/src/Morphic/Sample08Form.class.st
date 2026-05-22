"
This morph shows image rotating and scaling. Currently using WarpBlt. On top, some Vector Graphics.

See also class comment at Sample01Star.

Evaluate:
Sample08Form new openInWorld
"
Class {
	#name : 'Sample08Form',
	#superclass : 'PlacedMorph',
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'drawing' }
Sample08Form >> drawOn: aCanvas [

	aCanvas image: ImageMorph defaultForm at: -10@ -10.

	aCanvas strokeWidth: 4 color: (Color green alpha: 0.6) do: [
		aCanvas
			ellipseCenter: 36@60 radius: 8 @ 8 ].

	aCanvas strokeWidth: 10 color: (Color black alpha: 0.2) fillColor: (Color yellow alpha: 0.4) do: [
		aCanvas
			ellipseCenter: 36@60 radius: 24 @ 24 ].

]

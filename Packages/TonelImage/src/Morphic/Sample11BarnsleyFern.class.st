"
A Barnsley Fern.

See https://youtu.be/rkEnRrlHEZM?si=1SNqV1nz4iAeX-PS
See https://en.wikipedia.org/wiki/Barnsley_fern

Sample11BarnsleyFern new openInWorld

Thanks Leandro Caniglia for the suggestion!
"
Class {
	#name : 'Sample11BarnsleyFern',
	#superclass : 'PlacedMorph',
	#instVars : [
		'probabilities',
		'affineTransformations'
	],
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'drawing' }
Sample11BarnsleyFern >> drawOn: aCanvas [
	| tx p c |
	c := Color lightGreen alpha: 0.5.
	p := 0@0.
	40000 timesRepeat: [
		aCanvas strokeWidth: 1.2 color: c do: [
			aCanvas moveTo: p*40; lineTo: p*40.
			tx := Random withProbabilities: probabilities pickFrom: affineTransformations.
			p := tx transform: p ]]
]

{ #category : 'initialization' }
Sample11BarnsleyFern >> initialize [
	"See https://en.wikipedia.org/wiki/Barnsley_fern "
	super initialize.
	
	probabilities := #[ 0.01 0.85 0.07 0.07 ].
	affineTransformations := {
		"Stem"
		AffineTransformation from:
			#[ 	0.00 	0.00 	0.00
				0.00 	0.16 	0.00 ].
		"Successively smaller leaflets"
		AffineTransformation from:
			#[ 	0.85 	0.04 	0.00
				-0.04 	0.85 	1.60 ].
		"Largest left-hand leaflet"
		AffineTransformation from:
			#[ 	0.20 	-0.26 	0.00
				0.23 	0.22 	1.60 ].
		"Largest right-hand leaflet "
		AffineTransformation from:
			#[ 	-0.15 	0.28 	0.00
				0.26 	0.24 	0.44 ] }.
]

{ #category : 'drawing' }
Sample11BarnsleyFern >> yAxisPointsUp [
	"Follow the standard math convention, making increasing y values move upwards."

	^true
]

'From Cuis7.5 [latest update: #7423] on 19 August 2025 at 12:32:39 pm'!
!classDefinition: #Sample11BarnsleyFern category: #'Morphic-Examples'!
PlacedMorph subclass: #Sample11BarnsleyFern
	instanceVariableNames: 't1 p1 t2 p2 t3 p3 t4 p4 probabilities affineTransformations '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!

!Random class methodsFor: 'services' stamp: 'jmv 8/19/2025 12:23:10'!
withProbabilities: probabilities pickFrom: elements
	"probabilities and elements must be of same size.
	probabilities sum = 1.0"

	| r |
	r := self next.
	probabilities with: elements do: [ :p :e |
		r <= p ifTrue: [ ^e ].
		r := r - p ].
	
	self error: [ 'Probabilities do not add up to 1.0' ].! !


!Sample11BarnsleyFern methodsFor: 'initialization' stamp: 'jmv 8/19/2025 12:30:47'!
initialize
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
				0.26 	0.24 	0.44 ] }.! !

!Sample11BarnsleyFern methodsFor: 'drawing' stamp: 'jmv 8/19/2025 12:30:28'!
drawOn: aCanvas
	| tx p c |
	c := Color lightGreen alpha: 0.5.
	p := 0@0.
	40000 timesRepeat: [
		aCanvas strokeWidth: 1.2 color: c do: [
			aCanvas moveTo: p*40; lineTo: p*40.
			tx := Random withProbabilities: probabilities pickFrom: affineTransformations.
			p := tx transform: p ]]! !

!methodRemoval: Sample11BarnsleyFern #pickTransformation stamp: 'jmv 8/19/2025 12:30:37'!
Sample11BarnsleyFern removeSelector: #pickTransformation!
!classDefinition: #Sample11BarnsleyFern category: #'Morphic-Examples'!
PlacedMorph subclass: #Sample11BarnsleyFern
	instanceVariableNames: 'probabilities affineTransformations'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!

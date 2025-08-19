'From Cuis7.5 [latest update: #7419] on 19 August 2025 at 9:43:02 am'!
!classDefinition: #Sample11BarnsleyFern category: #'Morphic-Examples'!
PlacedMorph subclass: #Sample11BarnsleyFern
	instanceVariableNames: 't1 p1 t2 p2 t3 p3 t4 p4'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!

!Sample11BarnsleyFern commentStamp: '<historical>' prior: 0!
A Barnsley Fern.

See https://youtu.be/rkEnRrlHEZM?si=1SNqV1nz4iAeX-PS
See https://en.wikipedia.org/wiki/Barnsley_fern

Sample11BarnsleyFern new openInWorld

Thanks Leandro Caniglia for the suggestion!!!


!Sample11BarnsleyFern methodsFor: 'initialization' stamp: 'jmv 8/19/2025 09:07:24'!
initialize
	"See https://en.wikipedia.org/wiki/Barnsley_fern "
	super initialize.
	
	"Stem"
	t1 := AffineTransformation from:
		#[ 	0.00 	0.00 	0.00
			0.00 	0.16 	0.00 ].
	p1 := 0.01.
	
	"Successively smaller leaflets"
	t2 := AffineTransformation from:
		#[ 	0.85 	0.04 	0.00
			-0.04 	0.85 	1.60 ].
	p2 := 0.85.
	
	"Largest left-hand leaflet"
	t3 := AffineTransformation from:
		#[ 	0.20 	-0.26 	0.00
			0.23 	0.22 	1.60 ].
	p3 := 0.07.
	
	"Largest right-hand leaflet "
	t4 := AffineTransformation from:
		#[ 	-0.15 	0.28 	0.00
			0.26 	0.24 	0.44 ].
	p4 := 0.07.! !

!Sample11BarnsleyFern methodsFor: 'drawing' stamp: 'jmv 8/19/2025 09:22:50'!
drawOn: aCanvas
	| p c |
	c := Color lightGreen alpha: 0.5.
	p := 0@0.
	40000 timesRepeat: [
		aCanvas strokeWidth: 1.2 color: c do: [
			aCanvas moveTo: p*40; lineTo: p*40.
			p := self pickTransformation transform: p ]]! !

!Sample11BarnsleyFern methodsFor: 'drawing' stamp: 'jmv 8/19/2025 09:30:41'!
pickTransformation
	| r |
	r := Random next.
	
	r <= p1 ifTrue: [ ^t1 ].
	r := r - p1.
	
	r <= p2 ifTrue: [ ^t2 ].
	r := r - p2.
	
	r <= p3 ifTrue: [ ^t3 ].
	r := r - p3.
	
	r <= p4 ifTrue: [ ^t4 ].
	r := r - p4.
	
	self error: [ 'Probabilities do not add up to 1.0' ].! !

!Sample11BarnsleyFern methodsFor: 'drawing' stamp: 'jmv 8/19/2025 09:20:31'!
yAxisPointsUp
	"Follow the standard math convention, making increasing y values move upwards."

	^true! !


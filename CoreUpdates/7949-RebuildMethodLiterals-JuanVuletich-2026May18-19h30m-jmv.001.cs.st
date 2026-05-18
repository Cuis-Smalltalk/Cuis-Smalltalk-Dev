'From Cuis7.7 [latest update: #7948] on 18 May 2026 at 7:43:54 pm'!

!Sample11BarnsleyFern methodsFor: 'initialization' stamp: 'jmv 18/May/2026 19:39:02'!
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


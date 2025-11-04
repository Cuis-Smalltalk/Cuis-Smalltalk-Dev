'From Cuis7.5 [latest update: #7686] on 4 November 2025 at 12:14:20 pm'!

!HaloMorph methodsFor: 'drawing' stamp: 'jmv 11/4/2025 11:57:40'!
drawOn: aCanvas 

	target ifNil: [^super drawOn: aCanvas].
	
	"For debugging halos' morph bounds visually."
	
	"Fill the bounds of halo's morph. To activate:
	Preferences at: #halosMorphBoundsFillColor put: (Color blue alpha: 0.2)
	"
	(Preferences at: #halosMorphBoundsFillColor) ifNotNil: [:fillColor |
		aCanvas
			fillRectangle: self localBounds
			color: fillColor].
	
	"Draw a frame over halos morph bounds. To activate:
	Preferences at: #halosMorphBoundsFrameColor put: Color blue.
	"
	(Preferences at: #halosMorphBoundsFrameColor) ifNotNil: [:frameColor |
		aCanvas frameRectangle: self localBounds 
			borderWidth: 1 
			color: frameColor].
		
	target haloShowsCoordinateSystem ifTrue: [
		target displayFullBounds ifNotNil: [ :fb |
			aCanvas privateFrameGlobalRect: fb borderWidth: 3 color: `Color black alpha: 0.1`.
			target displayBounds ifNotNil: [ :b |
				b = fb ifFalse: [
					aCanvas privateFrameGlobalRect: b borderWidth: 3 color: `Color black alpha: 0.1` ].
				self drawCoordinateSystemOn: aCanvas ]]].! !


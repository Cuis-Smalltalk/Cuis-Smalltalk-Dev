'From Cuis 4.1 of 12 December 2012 [latest update: #1621] on 5 March 2013 at 8:16:11 am'!

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 3/3/2013 13:50'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			"This method should be redone calling reasonable protocol on the canvas.
			Also check use of global coordinates..."
			self flag: #jmvVer2.
			anchoredFormOrMorph morphPositionInWorld:
				((destX - anchoredFormOrMorph morphWidth)@
				(lineY+ line baseline - anchoredFormOrMorph morphHeight)) -
					paraTopLeft ]
		ifFalse: [
			destY _ lineY.
			runX _ destX.
			canvas
				image: anchoredFormOrMorph
				at: destX - anchoredFormOrMorph width @ (destY + line baseline - anchoredFormOrMorph height) ].
	^ true! !


'From Cuis 4.2 of 25 July 2013 [latest update: #2684] on 20 February 2016 at 4:20:06.954212 pm'!

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 2/20/2016 16:17'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			"This method should be redone calling reasonable protocol on the canvas.
			Also check use of global coordinates..."
			self flag: #jmvVer2.
			anchoredFormOrMorph privateOwner: (canvas instVarNamed: 'currentMorph').
			anchoredFormOrMorph morphPosition:
				((destX - anchoredFormOrMorph morphWidth)@
				(lineY+ line baseline - anchoredFormOrMorph morphHeight)) -
					topLeft.
			canvas fullDraw: anchoredFormOrMorph ]
		ifFalse: [
			destY _ lineY.
			canvas
				image: anchoredFormOrMorph
				at: destX - anchoredFormOrMorph width @ (destY + line baseline - anchoredFormOrMorph height) ].
	^ true! !


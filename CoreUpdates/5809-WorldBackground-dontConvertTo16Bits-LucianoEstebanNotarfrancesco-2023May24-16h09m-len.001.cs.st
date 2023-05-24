'From Cuis 6.0 [latest update: #5806] on 24 May 2023 at 4:16:48 pm'!

!PasteUpMorph methodsFor: 'misc' stamp: 'len 5/24/2023 16:16:25'!
buildMagnifiedBackgroundImage
	| image |
	backgroundImageData
		ifNil: [ backgroundImage := nil ]
		ifNotNil: [
			[
				backgroundImage := nil.
				Smalltalk primitiveGarbageCollect.
				image := Form fromBinaryStream: backgroundImageData readStream.
				backgroundImage := image magnifyTo: extent.
				"Save some memory. Enable if desired."
				"backgroundImage := backgroundImage orderedDither32To16 asColorFormOfDepth: 8."
				image := nil.
				Smalltalk primitiveGarbageCollect.
				backgroundImage bits pin.
			] on: Error do: [backgroundImage := nil]. "Can happen if JPEG plugin not built"
			self redrawNeeded
		]! !


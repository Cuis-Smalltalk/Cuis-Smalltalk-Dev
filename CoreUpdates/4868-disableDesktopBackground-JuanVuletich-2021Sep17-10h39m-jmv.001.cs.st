'From Cuis 5.0 [latest update: #4866] on 17 September 2021 at 10:58:32 am'!

!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 9/17/2021 10:58:19'!
buildMagnifiedBackgroundImage
	| image |
	backgroundImageData
		ifNil: [ backgroundImage _ nil ]
		ifNotNil: [
			[
				backgroundImage _ nil.
				Smalltalk primitiveGarbageCollect.
				image _ Form fromBinaryStream: backgroundImageData readStream.
				backgroundImage _ image magnifyTo: extent.
				backgroundImage _ backgroundImage orderedDither32To16 asColorFormOfDepth: 8.
				image _ nil.
				Smalltalk primitiveGarbageCollect.
				backgroundImage bits pin.
			] on: Error do: [backgroundImage := nil]. "Can happen if JPEG plugin not built"
			self redrawNeeded
		]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
self runningWorld color: (Color fromHexString: '#214A8C') lighter.!


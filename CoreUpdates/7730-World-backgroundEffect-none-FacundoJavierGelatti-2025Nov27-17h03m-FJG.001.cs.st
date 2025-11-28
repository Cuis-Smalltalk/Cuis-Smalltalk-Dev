'From Cuis7.5 [latest update: #7721] on 27 November 2025 at 5:16:46 pm'!

!PasteUpMorph methodsFor: 'drawing' stamp: 'FJG 10/31/2025 01:53:56'!
drawOn: aCanvas
	"draw background image"
	
	backgroundImage
		ifNil: [ super drawOn: aCanvas ]
		ifNotNil: [
			| effect |
			effect := PreferenceSet sysPreferences at: #backgroundEffect.

			effect = #none ifTrue: [ super drawOn: aCanvas ].
			
			effect = #tile
				ifFalse: [ aCanvas image: backgroundImage at: `0 @ 0` ]
				ifTrue: [
					| height width x y |
					height := backgroundImage height.
					width := backgroundImage width.
					x := 0.
					y := 0.
					[ x < extent x ] whileTrue: [
						[ y < extent y ] whileTrue: [
							aCanvas image: backgroundImage at: x @ y.
							y := y + height
						].
						x := x + width.
						y := 0
					]
				]
		]! !

!PasteUpMorph methodsFor: 'misc' stamp: 'FJG 10/31/2025 01:52:31'!
buildMagnifiedBackgroundImage
	"
	Here's an example::

	filePath := '/Users/volkmannm/Pictures/images/altitude1600.jpg'.
	PreferenceSet sysPreferences at: #backgroundEffect put: #tile.
	stream := filePath asFileEntry readStream.
	self runningWorld backgroundImageData: stream binary contentsOfEntireFile.

	You can also set the #backgroundEffect preference to #cover, #stretch or #none.
	"
	| effect image scale |
		
	backgroundImage := nil.
	backgroundImageData
		ifNil: [ self redrawNeeded ]
		ifNotNil: [
			[
 				Smalltalk primitiveGarbageCollect.
				image := Form fromBinaryStream: backgroundImageData readStream.
				effect := PreferenceSet sysPreferences at: #backgroundEffect.
				backgroundImage := effect caseOf: {
					[#cover] -> [
						scale := (extent x / image width) max: (extent y / image height).
						image magnifyBy: scale.
					].
					[#stretch] -> [image magnifyTo: extent]
				} otherwise: [ image "for #tile and #none" ].

				"Save some memory. Enable if desired."
 				"backgroundImage := backgroundImage orderedDither32To16 asColorFormOfDepth: 8."
				
				image := nil.
				Smalltalk primitiveGarbageCollect.
				backgroundImage bits pin.
				self redrawNeeded.
			] on: Error do: [backgroundImage := nil]. "Can happen if JPEG plugin not built"
		]! !


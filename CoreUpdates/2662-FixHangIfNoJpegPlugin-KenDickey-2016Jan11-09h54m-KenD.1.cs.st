'From Cuis 4.2 of 25 July 2013 [latest update: #2658] on 11 January 2016 at 9:54:26.250312 am'!

!PasteUpMorph methodsFor: 'misc' stamp: 'KenD 1/10/2016 11:43'!
buildMagnifiedBackgroundImage
	| image old |
	old _ backgroundImage.
	backgroundImageData
		ifNil: [ backgroundImage _ nil ]
		ifNotNil: [ 
			[image _ Form fromBinaryStream: backgroundImageData readStream.
			backgroundImage _ image magnifyTo: extent.
			self canvas ifNotNil: [ :c |
				(backgroundImage depth = 32 and: [ c depth < 32 ]) ifTrue: [
					backgroundImage _ backgroundImage orderedDither32To16 ]]
			] on: Error do: [backgroundImage := nil]. "Can happen if JPEG plugin not built"
		].
	old == backgroundImage ifFalse: [
		self redrawNeeded ]! !


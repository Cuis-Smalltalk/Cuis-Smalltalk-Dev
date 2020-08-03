'From Cuis 5.0 [latest update: #4315] on 28 July 2020 at 12:03:43 pm'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/28/2020 12:02:21'!
fullContainsGlobalPoint: worldPoint
	"Answer true even if aLocalPoint is in some submorph, regardless of being also inside our shape."

	"If not visible, won't contain any point at all."
	| canvas |
	self visible ifFalse: [ ^false ].

	canvas _ self world canvas.
	canvas isNil ifTrue: [ ^false ].
	(canvas morph: self isAtPoint: worldPoint) ifTrue: [ ^ true ]. 
	self submorphsDo: [ :m |
		(m fullContainsGlobalPoint: worldPoint) ifTrue: [ ^ true ]].
	^ false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/28/2020 12:01:54'!
fullContainsPoint: aLocalPoint
	"Answer true even if aLocalPoint is in some submorph, regardless of being also inside our shape."

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	^self fullContainsGlobalPoint: (self externalizeToWorld: aLocalPoint).! !


!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 7/28/2020 12:00:02'!
buildMagnifiedBackgroundImage
	| image old |
	old _ backgroundImage.
	backgroundImageData
		ifNil: [ backgroundImage _ nil ]
		ifNotNil: [ 
			[image _ Form fromBinaryStream: backgroundImageData readStream.
			backgroundImage _ image magnifyTo: extent.
			self canvas ifNotNil: [ :c |
				(backgroundImage depth = 32 and: [ c form depth < 32 ]) ifTrue: [
					backgroundImage _ backgroundImage orderedDither32To16 ]]
			] on: Error do: [backgroundImage := nil]. "Can happen if JPEG plugin not built"
		].
	old == backgroundImage ifFalse: [
		self redrawNeeded ]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
self runningWorld buildMagnifiedBackgroundImage; clearCanvas!


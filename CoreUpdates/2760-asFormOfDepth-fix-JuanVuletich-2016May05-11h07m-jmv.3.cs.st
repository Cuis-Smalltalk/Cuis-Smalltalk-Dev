'From Cuis 4.2 of 25 July 2013 [latest update: #2759] on 5 May 2016 at 4:20:47.597841 pm'!

!ColorForm methodsFor: 'converting' stamp: 'jmv 5/5/2016 16:08'!
asFormOfDepth: d
	| answer |
	d = depth ifTrue: [ ^self ].
	d abs = depth abs ifTrue: [
		answer _ self copy.
		answer swapEndianness.
		BitBlt swapBytesIn32BitWords: answer bits.
		^ answer ].
	^ super asFormOfDepth: d! !


!Form methodsFor: 'converting' stamp: 'jmv 5/5/2016 16:05'!
asFormOfDepth: d
	| newForm source |
	d = depth ifTrue: [ ^self ].
	source _ (self depth = 32 and: [ d abs < 32 ])
		ifTrue: [ self copy convertAlphaToZeroValueTransparency ]
		ifFalse: [ self ].
	newForm _ Form extent: source extent depth: d.
	(BitBlt toForm: newForm)
		colorMap: (source colormapIfNeededFor: newForm);
		copy: source boundingBox
		from: 0@0 in: source
		fillColor: nil rule: Form over.
	"If we build a 32bpp from one of smaller depth,
	it will have zero in the alpha channel (until BitBlt is fixed!!)"
	(newForm depth = 32 and: [self depth < 32]) ifTrue: [
		newForm fixAlpha ].
	^newForm! !


!ColorForm reorganize!
('accessing' colors colors:)
('displaying' maskingMap)
('pixel accessing' colorAt: colorAt:put: grayAt:put: isTransparentAt:)
('color manipulation' asGrayScale colormapIfNeededForDepth: indexOfColor: mapColor:to: replaceColorAtIndex:with:)
('copying' asCursorForm blankCopyOf:scaledBy: copy: postCopy)
('private' clearColormapCache ensureColorArrayExists setColors:cachedColormap:depth: setExtent:depth: unusedColormapEntry)
('scaling, rotation' flippedBy:centerAt:)
('fileIn/Out' readAttributesFrom: storeOn: writeAttributesOn:)
('testing' is: mightBeTranslucent)
('converting' asFormOfDepth:)
!


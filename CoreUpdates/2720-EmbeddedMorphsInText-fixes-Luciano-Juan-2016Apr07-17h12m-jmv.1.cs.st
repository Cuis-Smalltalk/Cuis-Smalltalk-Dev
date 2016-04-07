'From Cuis 4.2 of 25 July 2013 [latest update: #2719] on 7 April 2016 at 5:35:16.214925 pm'!

!InnerTextMorph methodsFor: 'notifications' stamp: 'jmv 4/7/2016 17:34'!
someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	super someSubmorphPositionOrExtentChanged.
	self releaseEditorAndTextComposition! !


!TextAnchor methodsFor: 'copying' stamp: 'jmv 4/7/2016 17:26'!
postCopy

	anchoredFormOrMorph _ anchoredFormOrMorph copy! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 4/7/2016 16:42'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
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


!Text methodsFor: 'copying' stamp: 'jmv 4/7/2016 17:25'!
postCopy

	string _ string copy.
	runs _ runs copy.
	runs mapValues: [ :attributes | attributes collect: [ :attr | attr copy ]]! !


!InnerTextMorph methodsFor: 'notifications' stamp: 'len 4/4/2016 06:52'!
possiblyChanged
	| embeddedMorphs |
	embeddedMorphs _ model actualContents embeddedMorphs.
	self submorphsDo: [:each| (embeddedMorphs includes: each) ifFalse: [each delete]].
	embeddedMorphs do: [:each| each owner == self ifFalse: [self addMorphFront: each]].
	owner possiblyChanged! !


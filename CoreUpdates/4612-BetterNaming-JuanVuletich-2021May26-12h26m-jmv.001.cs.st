'From Cuis 5.0 [latest update: #4611] on 26 May 2021 at 12:26:45 pm'!

!MorphicCanvas methodsFor: 'drawing - Global Coordinates' stamp: 'jmv 5/26/2021 12:12:14'!
frameReverseGlobalRect: r borderWidth: borderWidth
	self subclassResponsibility.! !


!BitBltCanvas methodsFor: 'drawing - Global Coordinates' stamp: 'jmv 5/26/2021 12:12:48'!
frameReverseGlobalRect: rect borderWidth: borderWidth
	"
	(BitBltCanvas onForm: Display)
		frameReverseGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	engine ifNil: [ ^nil ].
	engine
		sourceForm: nil;
		fillColor: `Color gray`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 5/26/2021 12:12:26'!
drawCurrentAsOutline

	self isCurrentMorphVisible ifTrue: [
		self frameReverseGlobalRect: currentMorph displayBounds borderWidth: 2.
		currentMorph displayBoundsSetFrom: self ].! !

!methodRemoval: BitBltCanvas #reverseRectangleBorder:borderWidth: stamp: 'jmv 5/26/2021 12:26:39'!
BitBltCanvas removeSelector: #reverseRectangleBorder:borderWidth:!
!methodRemoval: BitBltCanvas #reverseGlobalRectangleBorder:borderWidth: stamp: 'jmv 5/26/2021 12:26:39'!
BitBltCanvas removeSelector: #reverseGlobalRectangleBorder:borderWidth:!
!methodRemoval: MorphicCanvas #reverseRectangleBorder:borderWidth: stamp: 'jmv 5/26/2021 12:26:39'!
MorphicCanvas removeSelector: #reverseRectangleBorder:borderWidth:!
!methodRemoval: MorphicCanvas #reverseGlobalRectangleBorder:borderWidth: stamp: 'jmv 5/26/2021 12:26:39'!
MorphicCanvas removeSelector: #reverseGlobalRectangleBorder:borderWidth:!

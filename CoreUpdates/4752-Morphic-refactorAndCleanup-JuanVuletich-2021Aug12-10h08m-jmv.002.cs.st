'From Cuis 5.0 [latest update: #4750] on 12 August 2021 at 10:36:24 am'!

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 8/12/2021 10:35:37'!
boundsFinderCanvas
	^self! !


!MorphicCanvas class methodsFor: 'services' stamp: 'jmv 8/12/2021 09:46:30'!
isVectorGraphicsUsedForAllRendering
	"
	MorphicCanvas isVectorGraphicsUsedForAllRendering
	"
	^MorphicCanvas activeSubclass name == #VectorDrawingCanvas! !


'From Cuis 5.0 [latest update: #4594] on 11 May 2021 at 10:28:42 am'!

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 5/11/2021 09:51:57'!
withScale: aNumber position: otherPoint
	"
	(AffineTransformation withScale: 4 position: 0.2@0.7) transform: 1@1
	"
	^self new
		setPointScale: aNumber@aNumber;
		setTranslation: otherPoint! !


!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 5/11/2021 09:51:35'!
withPointScale: aPoint position: otherPoint
	"
	(AffineTransformation withPointScale: 4@3 position: 0.2@0.7) transform: 1@1
	"
	^self new
		setPointScale: aPoint;
		setTranslation: otherPoint! !


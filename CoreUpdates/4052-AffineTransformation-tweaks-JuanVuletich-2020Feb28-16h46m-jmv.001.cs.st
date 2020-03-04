'From Cuis 5.0 [latest update: #4048] on 28 February 2020 at 6:23:37 pm'!

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 2/28/2020 18:12:45'!
withPointScale: aPoint position: otherPoint
	"
	(AffineTransformation withPointScale: 4@3) transform: 1@1
	"
	^self new
		setPointScale: aPoint;
		setTranslation: otherPoint! !


!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 2/28/2020 18:15:20'!
withScale: aNumber
	^self new setPointScale: aNumber@aNumber! !


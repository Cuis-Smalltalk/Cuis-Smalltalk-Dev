'From Cuis6.3 [latest update: #6245] on 1 March 2024 at 3:02:53 pm'!

!Rectangle class methodsFor: 'instance creation' stamp: 'jmv 3/1/2024 15:02:44'!
center: centerPoint extent: extentPoint
	"Answer an instance of me whose center is centerPoint and width 
	by height is extentPoint.  "

	^self origin: centerPoint - (extentPoint/2) extent: extentPoint! !


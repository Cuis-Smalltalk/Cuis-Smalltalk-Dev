'From Cuis 5.0 [latest update: #4848] on 7 September 2021 at 11:05:59 am'!

!BitBltCanvas methodsFor: 'initialization' stamp: 'jmv 9/7/2021 09:41:09'!
resetCanvas
	"To be called in case of possible inconsistency due to an exception during drawing.
	See #displayWorldSafely"

	super resetCanvas.
	boundsFinderCanvas resetCanvas.! !


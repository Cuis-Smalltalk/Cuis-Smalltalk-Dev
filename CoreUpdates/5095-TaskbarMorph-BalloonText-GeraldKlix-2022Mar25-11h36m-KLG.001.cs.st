'From Cuis 6.0 [latest update: #5069] on 25 March 2022 at 11:37:28 am'!

!TaskbarMorph methodsFor: 'initialization' stamp: 'KLG 3/23/2022 22:49:02'!
balloonText
	"Answer the ballon text .
	
	Done dynamicly; System might change during my lifetime."

	^ 'About this system:' bold, `String newLineString`, Smalltalk systemInformationString! !


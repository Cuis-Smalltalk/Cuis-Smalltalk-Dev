'From Cuis 4.0 of 21 April 2012 [latest update: #1438] on 9 September 2012 at 12:43:38 am'!

!Form class methodsFor: 'fileIn/Out' stamp: 'jmv 9/9/2012 00:41'!
services
	"Currently, no services for bitmap graphic files
	(GIF, JPG, PNG, 'Form storeOn: (run coded)' and BMP)"
	^ #()! !

!methodRemoval: Form class #openAsBackground:!
Form class removeSelector: #openAsBackground:!
!methodRemoval: Form class #serviceImageAsBackground!
Form class removeSelector: #serviceImageAsBackground!
!methodRemoval: Form #setAsBackground!
Form removeSelector: #setAsBackground!

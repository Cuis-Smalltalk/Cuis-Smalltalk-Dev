'From Cuis 5.0 of 7 November 2016 [latest update: #3562] on 2 January 2019 at 7:08:02 pm'!

!FileList methodsFor: 'own services' stamp: 'HAW 1/2/2019 19:06:52'!
serviceGetHex

	^ SimpleServiceEntry 
		provider: self 
		label: 'view as hex' 
		selector: #getHex
		description: 'view as hex'
		icon: #preferencesDesktopFontIcon! !


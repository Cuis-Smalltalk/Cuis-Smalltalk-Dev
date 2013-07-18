'From Cuis 4.1 of 12 December 2012 [latest update: #1641] on 22 March 2013 at 9:01:57 am'!

!FormCanvas methodsFor: 'copying' stamp: 'jmv 3/22/2013 09:00'!
copyClipRect: aRectangle
	| o |
	o _ self canvasOrigin.
	^ self clone
		resetGrafPort
			setOrigin: o
			clipRect: (clipRect intersect: (aRectangle translatedBy: o))
! !

!methodRemoval: FormCanvas #copy!
FormCanvas removeSelector: #copy!
!methodRemoval: FormCanvas #copyOrigin:clipRect:!
FormCanvas removeSelector: #copyOrigin:clipRect:!

'From Cuis 4.1 of 12 December 2012 [latest update: #1652] on 1 April 2013 at 3:07:06 pm'!

!FormCanvas methodsFor: 'copying' stamp: 'jmv 4/1/2013 15:00'!
copyClipRect: aRectangle
	| o |
	o _ self canvasOrigin.
	^ self clone
		resetGrafPort
		setOrigin: o;
		setClipRect: (clipRect intersect: (aRectangle translatedBy: o))
! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 4/1/2013 15:02'!
resetGrafPort
	"Private!! Create a new grafPort for a new copy."

	port _ GrafPort toForm: form! !


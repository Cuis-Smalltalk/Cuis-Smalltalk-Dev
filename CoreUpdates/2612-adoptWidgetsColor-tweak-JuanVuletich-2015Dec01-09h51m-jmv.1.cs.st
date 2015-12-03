'From Cuis 4.2 of 25 July 2013 [latest update: #2610] on 1 December 2015 at 9:55:43.307509 am'!

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 12/1/2015 09:53'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	doAdoptWidgetsColor
		ifTrue: [ self color: (Theme current buttonColorFrom: paneColor) ]
		ifFalse: [ self color: Color transparent ]! !


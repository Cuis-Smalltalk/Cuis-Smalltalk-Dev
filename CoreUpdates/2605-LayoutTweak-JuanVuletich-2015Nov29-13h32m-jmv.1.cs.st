'From Cuis 4.2 of 25 July 2013 [latest update: #2602] on 29 November 2015 at 1:33:29.810299 pm'!

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 11/29/2015 13:33'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	doAdoptWidgetsColor 
		ifTrue: [ self color: (Theme current buttonColorFrom: paneColor) ]
		ifFalse: [ self color: Color transparent ]! !


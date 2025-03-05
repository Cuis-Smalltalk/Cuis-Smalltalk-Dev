'From Cuis7.3 [latest update: #7062] on 5 March 2025 at 10:56:53 am'!

!LayoutMorph methodsFor: 'accessing' stamp: 'hlsf 2/9/2025 10:11:59'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	doAdoptWidgetsColor
		ifTrue: [ self color: (Theme current buttonColorFrom: color) ]
		ifFalse: [ self color: `Color transparent` ]! !


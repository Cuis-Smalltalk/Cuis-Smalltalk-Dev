'From Cuis7.9 [latest update: #8026] on 26 June 2026 at 12:18:49 am'!

!LayoutMorph methodsFor: 'accessing' stamp: 'len 25/Jun/2026, 5:16:41 pm (UTC)'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	adoptsWidgetsColor
		ifTrue: [ self color: (Theme current buttonColorFrom: color) ]! !

!LayoutMorph methodsFor: 'initialization' stamp: 'len 25/Jun/2026, 4:43:00 pm (UTC)'!
defaultColor
	^ Color transparent! !


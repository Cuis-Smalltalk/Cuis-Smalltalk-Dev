'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 7 June 2016 at 10:50:21.62442 pm'!

!SystemWindow methodsFor: 'drawing' stamp: 'len 5/27/2016 21:56'!
drawOn: aCanvas

	| titleColor roundCorners |

	titleColor _ self widgetsColor.
	self isTopWindow
		ifTrue: [ titleColor _ titleColor lighter ].

	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			"Round corners. Optional title gradient."
			self drawRoundedFrameOn: aCanvas color: titleColor ]
		ifFalse: [
			"No round corners. No title gradient."
			self drawClassicFrameOn: aCanvas color: titleColor ].
	Theme current minimalWindows
		ifFalse: [self drawLabelOn: aCanvas]! !


'From Cuis 5.0 of 7 November 2016 [latest update: #3400] on 9 August 2018 at 6:13:10 pm'!

!Form methodsFor: 'displaying' stamp: 'jmv 8/9/2018 16:23:10'!
displayAutoRangeAt: aPoint
	"Display receiver, mapping used range to available gray levels"

	^ self displayAutoRangeAt: aPoint zoom: 1! !

!Form methodsFor: 'displaying' stamp: 'jmv 8/9/2018 16:22:37'!
displayAutoRangeAt: aPoint zoom: scale
	"Display receiver, compatibility with Matrix and subclasses such as FloatImage"

	| form |
	form _ self.
	scale = 1 ifFalse: [
		form _ form magnifyBy: scale ].
	form displayAt: aPoint.
	^ form! !


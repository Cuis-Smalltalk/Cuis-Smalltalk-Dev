'From Cuis7.5 [latest update: #7404] on 12 August 2025 at 3:22:39 pm'!

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'hlsf 8/12/2025 18:05:33'!
icon: aForm
	icon := aForm.
	magnifiedIcon := nil.
	self isRound ifTrue: [self morphExtent: icon extent + 5].
	self redrawNeeded.! !


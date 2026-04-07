'From Cuis7.7 [latest update: #7851] on 7 April 2026 at 11:52:45 am'!

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'DF 7/Apr/2026 11:52:28'!
icon: aForm
	icon := aForm.
	magnifiedIcon := nil.
	aForm notNil and:[self isRound] :: ifTrue:[self morphExtent: icon extent + 5].
	self redrawNeeded.! !


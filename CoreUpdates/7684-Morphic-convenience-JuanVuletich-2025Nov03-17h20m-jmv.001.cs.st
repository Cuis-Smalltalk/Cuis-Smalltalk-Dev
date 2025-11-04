'From Cuis7.5 [latest update: #7683] on 3 November 2025 at 5:20:58 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 11/3/2025 16:40:12'!
getKeyboardFocus
	self world activeHand newKeyboardFocus: self! !


!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 11/3/2025 16:28:42'!
scaleToFitInto: aRectangle
	"Scale and position ourselves in such way that our #findFullBoundsInOwner are in aRectangle.
	BoxMorph, and maybe others will not scale, but resize, changing the actual extent."

	| currentBounds scale slackForAntiAliasing |
	currentBounds := self findFullBoundsInOwner.
	slackForAntiAliasing := 0.
	scale := (aRectangle width-slackForAntiAliasing / (currentBounds width-slackForAntiAliasing))
		min: (aRectangle height-slackForAntiAliasing / (currentBounds height-slackForAntiAliasing)).
	scale := scale asFloat.
	self scaleBy: scale.

	currentBounds := self findFullBoundsInOwner.
	self morphPosition: aRectangle origin - (currentBounds origin - self morphPosition) + (aRectangle extent - currentBounds extent // 2).! !


!PlacedMorph methodsFor: 'geometry' stamp: 'jmv 11/3/2025 16:28:59'!
fitInto: aRectangle
	"Scale and position ourselves in such way that our #findFullBoundsInOwner are in aRectangle.
	BoxMorph, and maybe others will not scale, but resize, changing the actual extent."

	self scaleToFitInto: aRectangle.! !


'From Cuis7.5 [latest update: #7655] on 14 October 2025 at 9:21:12 am'!

!Theme methodsFor: 'other options' stamp: 'jmv 10/14/2025 09:17:55'!
fullScreenDeskMargin
	^ (Preferences at: #fullScreenLeavesDeskMargins) ifTrue: [64] ifFalse: [0]! !


!BoxMorph methodsFor: 'private' stamp: 'jmv 10/14/2025 09:17:21'!
fitInOwner

	| delta trialRect |
	trialRect := Rectangle origin: self morphPosition extent: extent.
	delta := trialRect amountToTranslateWithin: (owner displayBounds insetBy: Theme current fullScreenDeskMargin) .
	self morphPosition: trialRect origin + delta.! !


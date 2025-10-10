'From Cuis7.5 [latest update: #7650] on 10 October 2025 at 8:41:21 pm'!

!BoxMorph methodsFor: 'private' stamp: 'jmv 10/10/2025 20:40:02'!
fitInOwner

	| delta trialRect |
	trialRect := Rectangle origin: self morphPosition extent: extent.
	delta := trialRect amountToTranslateWithin: (owner displayBounds insetBy: extent // 4).
	self morphPosition: trialRect origin + delta.! !


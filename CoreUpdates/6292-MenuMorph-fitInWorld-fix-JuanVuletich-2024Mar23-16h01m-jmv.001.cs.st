'From Cuis6.3 [latest update: #6291] on 23 March 2024 at 4:09:10 pm'!

!MenuMorph methodsFor: 'private' stamp: 'jmv 3/23/2024 16:08:47'!
fitInWorld

	| delta trialRect |
	trialRect := Rectangle origin: self morphPosition extent: extent.
	delta := trialRect amountToTranslateWithin: owner displayBounds.
	self whenUIinSafeState: [self morphPosition: trialRect origin + delta].! !


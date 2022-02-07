'From Cuis 6.0 [latest update: #5062] on 7 February 2022 at 11:18:53 am'!

!Browser methodsFor: 'self-updating' stamp: 'len 2/5/2022 05:17:22'!
updateIfNeeded
	super updateIfNeeded.
	self changed: #systemCategoryRoots! !


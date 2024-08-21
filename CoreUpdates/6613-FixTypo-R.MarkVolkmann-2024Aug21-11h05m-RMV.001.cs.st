'From Cuis7.1 [latest update: #6611] on 21 August 2024 at 11:06:33 am'!

!BoxedMorph methodsFor: 'accessing' stamp: 'RMV 8/21/2024 11:06:15'!
borderWidth: anInteger
	borderWidth = anInteger ifFalse: [
		borderWidth := anInteger max: 0.
		self  ensureMinimumExtent;
			redrawNeeded ].! !

!BoxedMorph methodsFor: 'geometry' stamp: 'RMV 8/21/2024 11:06:01'!
ensureMinimumExtent

	| oldExtent |
	oldExtent := extent.
	(self privateExtent: extent) ifTrue: [
		self extentChanged: oldExtent ].! !

!methodRemoval: BoxedMorph #ensureMinimimExtent stamp: 'RMV 8/21/2024 11:06:17'!
BoxedMorph removeSelector: #ensureMinimimExtent!

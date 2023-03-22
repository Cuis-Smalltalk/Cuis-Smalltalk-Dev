'From Cuis 6.0 [latest update: #5708] on 21 March 2023 at 11:15:34 am'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 3/21/2023 11:14:55'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch."
	
	prevFullBounds ifNotNil: [
		aCanvas restorePatch: savedPatch bounds: prevFullBounds.
		(prevFullBounds containsRect: self displayFullBounds)
			ifFalse: [ self redrawNeeded ].
		submorphs isEmpty ifTrue: [
			prevFullBounds := nil ]].! !


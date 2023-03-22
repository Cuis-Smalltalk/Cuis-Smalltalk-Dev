'From Cuis 6.0 [latest update: #5714] on 22 March 2023 at 11:42:34 am'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 3/22/2023 11:39:29'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch."
	
	prevFullBounds ifNotNil: [
		aCanvas restorePatch: savedPatch bounds: prevFullBounds.

		"A full redraw may be needed if any contained morph could draw outside the bounds it had
		when the patch was saved.
		Avoiding if for SystemWindows improves resposiveness of the UI when many windows are open.
		It can be considered safe, as this only assumes that SystemWindows
			- don't resize themselves when carried by the hand
			- don't have protruding submorphs (see #submorphsMightProtrude)"
		(prevFullBounds containsRect: self displayFullBounds) ifFalse: [
			(submorphs allSatisfy: [ :m | m is: #SystemWindow]) ifFalse: [
				self redrawNeeded ]].

		submorphs isEmpty ifTrue: [
			prevFullBounds := nil ]].! !


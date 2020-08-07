'From Cuis 5.0 [latest update: #4344] on 6 August 2020 at 5:47:04 pm'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 8/6/2020 17:46:03'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."
	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas restorePatch: savedPatch.
		submorphs notEmpty ifTrue: [ ^ self ].
		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self
			invalidateDisplayRect: (self morphPosition extent: extent)
			fromSubmorph: nil
			for: nil.
		Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
		"show hardware cursor"
		savedPatch _ nil ].! !


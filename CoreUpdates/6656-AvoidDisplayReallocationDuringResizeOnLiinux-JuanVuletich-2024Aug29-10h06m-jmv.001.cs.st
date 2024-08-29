'From Cuis7.1 [latest update: #6655] on 29 August 2024 at 10:08:41 am'!

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 8/29/2024 10:06:49'!
setupDisplay
	"
	DisplayScreen setupDisplay.
	Display forceToScreen.
	"

	self terminateScreenUpdater.
	Display ifNil: [
		Smalltalk at: #Display put: DisplayScreen new.
		"See #recreateSpecialObjectsArray"
		Smalltalk specialObjectsArray at: 15 put: Display ].
	[
		Display setExtent: self actualScreenSize depth: Display nativeDepth.
		"{'New Smalltalk Display:'. self actualScreenSize} print."
	] on: OutOfMemory do: [
		"In case of memory emergency, use a much smaller Display"
		'Display allocation failed - Using Emergency (small) Display.'  print.
		'Resize Display (or OS Window) to retry Display allocation.'  print.
		Display setExtent: self actualScreenSize // 4 depth: Display nativeDepth ].! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 8/29/2024 10:07:14'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e p needsNewUIProcessAndCanvas |

	"Linux VMs continuously update #actualScreenSize while the user is resizing the OS window.
	Avoid reallocation of Display until the user is done with that.
	Currently, wait until a real event happens inside Cuis.
	This requires the user to move the mouse back into Cuis.
	Maybe other ways are possible."
	lastCycleHadAnyEvent ifFalse: [
		Smalltalk osVersion = 'linux-gnu' ifTrue: [ 		^self ]].

	needsNewUIProcessAndCanvas := false.
	DisplayScreen isDisplayExtentOk ifFalse: [
		"''print. {DateAndTime now. 'Smalltalk Display:'. Display extent. 'os display:'. DisplayScreen actualScreenSize} print."
		"If host OS scale factor has changed, try to recover our previous Display extent"
		newScaleFactor := DisplayScreen hostOSScaleFactor.
		newScaleFactor = ScaleFactor ifFalse: [
			ScaleFactor := newScaleFactor.
			e := DisplayScreen hostOSWindowExtent ifNotNil: [ :ee |
				(ee * ScaleFactor - DisplayScreen actualScreenSize + Display extent) truncated ].
			p := DisplayScreen hostOSWindowPosition.
			e ifNotNil: [
				"{'New ScaleFactor'. ScaleFactor. 'Set os display to:'. e} print."
				DisplayScreen hostOSWindowExtent: e].
			p ifNotNil: [DisplayScreen hostOSWindowPosition: p].
		].
		"When moving the Cuis window, for instance, between a laptop screen with ScaleFactor=2.0 and an external screen with
		ScaleFactor=1.0, usually the #hostOSWindowExtent: already left us with isDisplayExtentOk, so asking again is in order."
		DisplayScreen isDisplayExtentOk ifFalse: [
			"This allocates new memory for Display and is critical."
			needsNewUIProcessAndCanvas := true.
			"This may help with memory fragmentation issues possibly making new display allocation fail."
			canvas unpinAndFreeMemory.
			DisplayScreen setupDisplay ]].
	(needsNewUIProcessAndCanvas or: [canvas isConsistentWithTarget not])
		ifTrue: [
			self whenUIinSafeState: [ Cursor defaultCursor activateCursor ].
			"The current canvas is not consistent with Display, and therefore can no longer be used. A new one is required.
			Freeeing the memory it uses may help with memory fragmentation issues possibly making new canvas allocation fail.
			(In any case, if ther is not enough free memory for VectorCanvas and engine, use a BitBltCanvas, see #getMainCanvas)"
			canvas unpinAndFreeMemory.
			"Create a new UI process, including a new canvas consistent with updated Display.
			This will also terminate the current process, and this is needed because the current canvas can no longer be used.
			A canvas and vector engine inconsistent with Display could cause invalid memory accesses, possibly crashing the system."
			UISupervisor spawnNewMorphicProcessFor: self andTerminateCurrent: true ].! !


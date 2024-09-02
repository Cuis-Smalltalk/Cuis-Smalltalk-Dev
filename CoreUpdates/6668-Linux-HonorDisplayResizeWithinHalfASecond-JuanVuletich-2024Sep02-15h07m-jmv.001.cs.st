'From Cuis7.1 [latest update: #6667] on 2 September 2024 at 3:09:11 pm'!

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/2/2024 15:08:46'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e p needsNewUIProcessAndCanvas |

	"Linux VMs continuously update #actualScreenSize while the user is resizing the OS window.
	Try to avoid reallocation of Display until the user is done with that.
	For that, wait until a real event happens inside Cuis. (This requires the user to move the mouse back into Cuis).
	Or use a timer, and do it no more often than every half a second."
	DisplayScreen isDisplayExtentOk ifFalse: [
		Smalltalk osVersion = 'linux-gnu' ifTrue: [
			self valueOfProperty: #startForNewDisplayTimeout ifAbsentPut: [Time localMillisecondClock].
			(lastCycleHadAnyEvent not and: [ Time localMillisecondClock - (self valueOfProperty: #startForNewDisplayTimeout) < 500 ])
				ifTrue: [ ^self ].
			self removeProperty: #startForNewDisplayTimeout ]].

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
			(If there is not enough free memory for VectorCanvas and engine, use a BitBltCanvas, see #getMainCanvas)"
			canvas unpinAndFreeMemory.
			"Create a new UI process, including a new canvas consistent with updated Display.
			This will also terminate the current process, and this is needed because the current canvas can no longer be used.
			A canvas + vector engine inconsistent with Display will cause invalid memory accesses, crashing the system."
			UISupervisor spawnNewMorphicProcessFor: self andTerminateCurrent: true ].! !


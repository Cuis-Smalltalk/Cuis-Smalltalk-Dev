'From Cuis7.5 [latest update: #7300] on 1 July 2025 at 4:49:08 pm'!

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 7/1/2025 16:44:24'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e p needsNewUIProcessAndCanvas |

	"Linux and MacOS VMs continuously update #actualScreenSize while the user is resizing the OS window.
	Try to avoid reallocation of Display until the user is done with that.
	For that, wait until a real event happens inside Cuis. (This requires the user to move the mouse back into Cuis).
	Or use a timer, and do it no more often than every quarter a second, and only if the resize finishes/pauses for that time."
	DisplayScreen isDisplayExtentOk ifFalse: [
		Smalltalk platformName = 'Win32' ifFalse: [
			self valueOfProperty: #startForNewDisplayTimeout ifAbsentPut: [Time localMillisecondClock].
			self valueOfProperty: #newDisplayOldExtent ifAbsentPut: [DisplayScreen actualScreenSize].
			lastCycleHadAnyEvent ifFalse: [
				(self valueOfProperty: #newDisplayOldExtent) = DisplayScreen actualScreenSize ifFalse: [
					"If user still resizing, restart timer: wait until the user finishes resizing."
					self setProperty: #startForNewDisplayTimeout toValue: Time localMillisecondClock.
					self setProperty: #newDisplayOldExtent toValue: DisplayScreen actualScreenSize].
				Time localMillisecondClock - (self valueOfProperty: #startForNewDisplayTimeout) < 250 ifTrue: [
					^self ]].
			self removeProperty: #startForNewDisplayTimeout.
			self removeProperty: #newDisplayOldExtent ]].

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
			"Create a new UI process, including a new canvas consistent with updated Display."
			UISupervisor spawnNewMorphicProcessFor: self createNewCanvas: true.
			"Terminate the current process (the old UI process). This is needed because the old canvas can no longer be used:
			A canvas + vector engine inconsistent with Display will cause invalid memory accesses, crashing the system."
			Processor terminateActive.
			].! !


!HandMorph methodsFor: 'event handling' stamp: 'jmv 7/1/2025 16:27:55'!
processEventQueue
	"Process user input events from the local input devices.
	Answer true if any event was handled (but ignore MouseMove)"

	| evt evtBuf type hadAny mcs hadAnyMouseEvent hadAnyMouseMoveEvent |
	mcs := mouseClickState.
	hadAny := false.
	hadAnyMouseEvent := false.
	hadAnyMouseMoveEvent := false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		type := evtBuf first.
		evt := self createEventFrom: evtBuf ofType: type.
		evt ifNotNil: [
			"Finally, handle it. But process only up to one mouseMove per cycle. Discard the rest."
			(evt isMouseMove and: [ hadAnyMouseMoveEvent ]) ifFalse: [
				self startEventDispatch: evt.
				(evt isMouse or: [ evt isKeyboard ]) ifTrue: [
					hadAny := true ].
				evt isMouse ifTrue: [
					hadAnyMouseEvent := true.
					evt isMouseMove ifTrue: [
						hadAnyMouseMoveEvent := true ]]]]].
	hadAnyMouseEvent ifFalse: [
		mcs 
			ifNotNil: [ 
				"No mouse events during this cycle. Make sure click states time out accordingly"
				mcs
					handleEvent: lastMouseEvent asMouseMove
					from: self ]].
	^hadAny! !


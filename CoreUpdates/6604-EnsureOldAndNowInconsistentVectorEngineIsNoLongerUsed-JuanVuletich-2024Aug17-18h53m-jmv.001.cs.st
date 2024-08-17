'From Cuis7.1 [latest update: #6603] on 17 August 2024 at 7:02:36 pm'!

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 8/17/2024 18:56:44'!
spawnNewMorphicProcessFor: guiRootObject andTerminateCurrent: aBoolean
	"
	aBoolean ifTrue terminate current process. Use with care.
	Maybe prefer the safer #spawnNewMorphicProcessFor:
	"
	| doYield |
	doYield := false.
	"If the old one gets resumed, don't step on each other feet"
	UIProcess ifNotNil: [
		UIProcess
			name: 'Formerly: ', UIProcess name;
			priority: UIProcess priority-1.
		UIProcess isRunning ifTrue: [ doYield := true ]].
	UI := guiRootObject.
	UIProcess := guiRootObject runProcess.
	UIProcess resume.
	aBoolean
		ifFalse: [	doYield ifTrue: [ Processor yield ]]
		ifTrue: [ Processor terminateActive ].! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 8/17/2024 19:01:15'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e p needsNewUIProcessAndCanvas |
	needsNewUIProcessAndCanvas := false.
	DisplayScreen isDisplayExtentOk ifFalse: [
		"If host OS scale factor has changed, try to recover our previous Display extent"
		newScaleFactor := DisplayScreen hostOSScaleFactor.
		newScaleFactor = ScaleFactor ifFalse: [
			ScaleFactor := newScaleFactor.
			e := DisplayScreen hostOSWindowExtent ifNotNil: [ :ee |
				ee * ScaleFactor - DisplayScreen actualScreenSize + Display extent ].
			p := DisplayScreen hostOSWindowPosition.
			e ifNotNil: [DisplayScreen hostOSWindowExtent: e].
			p ifNotNil: [DisplayScreen hostOSWindowPosition: p].
		].
		"This allocates new memory for Display and is critical."
		needsNewUIProcessAndCanvas := true.
		"This may help with memory fragmentation issues possibly making new display allocation fail."
		canvas unpinAndFreeMemory.
		DisplayScreen setupDisplay ].
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


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 8/17/2024 18:54:53'!
spawnNewMorphicProcessFor: guiRootObject
	"
	Safe to run anytime, as the old Morphic process will end by itself.
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.
	"
	self spawnNewMorphicProcessFor: guiRootObject andTerminateCurrent: false.! !


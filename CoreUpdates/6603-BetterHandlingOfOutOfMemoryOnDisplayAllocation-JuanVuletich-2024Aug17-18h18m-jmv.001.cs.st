'From Cuis7.1 [latest update: #6598] on 17 August 2024 at 6:44:22 pm'!

!MorphicCanvas methodsFor: 'other' stamp: 'jmv 8/17/2024 18:37:41'!
unpinAndFreeMemory! !


!VectorCanvas methodsFor: 'other' stamp: 'jmv 8/17/2024 18:37:46'!
unpinAndFreeMemory
	engine unpinAndFreeMemory! !


!HybridCanvas methodsFor: 'other' stamp: 'jmv 8/17/2024 18:37:51'!
unpinAndFreeMemory
	vectorCanvas unpinAndFreeMemory! !


!VectorEngineDrawer methodsFor: 'services' stamp: 'jmv 8/17/2024 18:39:37'!
unpinAndFreeMemory

	"Unpin, if possible, any pinned large arrays."
	targetForm ifNotNil: [
		targetForm == Display ifFalse: [
			targetForm bits unpin ]].
	morphIds ifNotNil: [ morphIds unpin ].
	edgeCounts ifNotNil: [ edgeCounts unpin ].
	alphaMask ifNotNil: [ alphaMask unpin ].
	contour ifNotNil: [ contour unpin ].

	targetForm := nil.
	morphIds := nil.
	edgeCounts := nil.
	alphaMask := nil.
	contour := nil.! !


!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 8/17/2024 18:29:49'!
isDisplayExtentOk
	"False if Cuis main window size has changed, but Display hasn't been updated yet."

	Display extent = DisplayScreen actualScreenSize ifTrue: [ ^true ].

	"Note: We assume here that this condition means that in last #setupDisplay a memory allocation
	problem occured, and we are using a 1/4 emergency Display.
	If this is the case, don't try to allocate a new Display unless the OS Display is resized again."
	Display extent = (DisplayScreen actualScreenSize // 4) ifTrue: [ ^true ].

	^false! !

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 8/17/2024 18:31:34'!
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
		Display setExtent: self actualScreenSize depth: Display nativeDepth
	] on: OutOfMemory do: [
		"In case of memory emergency, use a much smaller Display"
		'Display allocation failed - Using Emergency (small) Display.'  print.
		'Resize Display (or OS Window) to retry Display allocation.'  print.
		Display setExtent: self actualScreenSize // 4 depth: Display nativeDepth ].! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 8/17/2024 18:44:13'!
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
			"This may help with memory fragmentation issues possibly making new canvas allocation fail.
			In any case, if ther is not enough free memory for VectorCanvas and engine, use BitBltCanvas.
			See #getMainCanvas"
			canvas unpinAndFreeMemory.
			UISupervisor newUIProcess ].! !


'From Cuis 5.0 [latest update: #4862] on 16 September 2021 at 11:30:30 am'!

!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 9/16/2021 11:22:16'!
setupDisplay: doGarbageCollection
	"
	DisplayScreen setupDisplay: true.
	Display forceToScreen.
	"

	self terminateScreenUpdater.
	doGarbageCollection ifTrue: [
		Display setExtent: 0@0 depth: 0 bits: nil.
		Smalltalk garbageCollect ].
	Display setExtent: self actualScreenSize depth: Display nativeDepth.
	Display beDisplay.
	self installScreenUpdater.! !


!DisplayScreen methodsFor: 'other' stamp: 'jmv 9/16/2021 10:58:07'!
forceToScreen: aRectangle
	"Force the given rectangular section of the Display to be copied to the screen. The primitive call does nothing if the primitive is not implemented. Used when the deferUpdates flag in the virtual machine is on; see #deferUpdates:.
	
	If for whatever reason, actual OS or hardware Display is smaller than us, don't go outside its bounds.
	This can sometimes happen, at least on MacOS, when frantically resizing the main OS Cuis window,
	while Morphic is busy drawing many heavy morphs: it could be observed than apparently in #displayWorld,
	after we were updated in #checkForNewScreenSize, MacOS window could be made smaller than aRectangle,
	and a hard crash due to an invalid memory access happened in this primitive.
	
	Protecting against our bounds being smaller than aRectangle is done in the primitive. No need to do it here."

	| platformDisplayExtent |
	platformDisplayExtent _ DisplayScreen actualScreenSize.
	self primShowRectLeft: (aRectangle left max: 0)
		right: (aRectangle right min: platformDisplayExtent x)
		top: (aRectangle top max: 0)
		bottom: (aRectangle bottom min: platformDisplayExtent y).
! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 9/16/2021 11:22:01'!
startUp
	"
	DisplayScreen startUp.
	Display forceToScreen.
	"
	self setupDisplay: false.! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/16/2021 11:25:32'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	DisplayScreen isDisplayExtentOk ifFalse: [
		"Minimize the risk of going out of memory:
		- First clear existing canvas, to free the memory it uses.
		- Then, setup the display, with a GarbageCollection prior to allocating new display memory.
		- Then set up new canvas."
		self clearCanvas.
		DisplayScreen setupDisplay: true.
		self setMainCanvas.
		self whenUIinSafeState: [ Cursor defaultCursor activateCursor ]].! !


'From Cuis6.0 [latest update: #6114] on 11 December 2023 at 4:56:19 pm'!
!classDefinition: #DisplayScreen category: #'Graphics-Display Objects'!
Form subclass: #DisplayScreen
	instanceVariableNames: ''
	classVariableNames: 'Damage ScreenUpdaterProcess ScreenUpdaterSemaphore CurrentDisplayBits '
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

!DisplayScreen methodsFor: 'private' stamp: 'jmv 12/11/2023 16:05:33'!
setExtent: extent depth: bitsPerPixel bits: bitmap
	"Please call #setExtent:depth:
	Or eventually implement this method like that."

	self shouldNotImplement.! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/11/2023 16:44:29'!
snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: clearAllStateFlag
	"This is the main method for image save and / or quit.
	See senders."
	"WARNING: Current process will be killed. UI Process will be restarted"
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic
	checks for a document file to process when starting up."
	"
	To test the full cleanup and startup procedures, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: true

	To test the cleanup done when saving the image, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: false
	"
	| activeProc |
	activeProc := Processor activeProcess.
[
	| isARealStartup guiRootObject guiRootObjectClass |
	save not & quit
		ifTrue: [ (SourceFiles at: 2) ifNotNil: [ :changes |
				ChangesInitialFileSize ifNotNil: [ changes truncate: ChangesInitialFileSize ]]]
		ifFalse: [ self logSnapshot: save andQuit: quit ].
	clearAllStateFlag ifTrue: [
		TranscriptWindow allInstancesDo: [ :each |
			each isInWorld ifTrue: [ each delete.]].
		UISupervisor ui tearDownDesktop.
		Transcript logToFile: false ].
	ActiveModel flushEventSystem.
	self processShutDownList: quit.
	"These could be moved to some #shutDown"
	Smalltalk stopLowSpaceWatcher.
	WeakArray stopFinalizationProcess.
	ProcessorScheduler stopBackgroundProcess.
	"Stuff needed to relaunch UI on startup"
	guiRootObjectClass := UISupervisor ui class.
	"Replace with this to create a new world at startup after 'saveAsNewVersion'"
	guiRootObject := clearAllStateFlag ifFalse: [ UISupervisor ui ].

	UISupervisor stopUIProcess.
	activeProc isTerminated ifFalse: [ activeProc terminate ].
	guiRootObject ifNotNil: [ guiRootObject releaseCachedState ].
	clearAllStateFlag ifTrue: [
		Processor processesDo: [ :p |
			(p name = 'Formerly: Morphic UI') ifTrue: [ p terminate ]]].
	MorphicCanvas allSubInstancesDo: [ :c |
		(c ownerProcess notNil and: [c ownerProcess isTerminated]) ifTrue: [
			c releaseEngine			]].

	"Clean Globals"
	Smalltalk at: #Sensor put: nil.
	Smalltalk closeSourceFiles.
	Smalltalk at: #SourceFiles put: nil.
	Smalltalk allClassesDo: [ :cls |
		cls releaseClassCachedState ].
	clearAllStateFlag ifTrue: [
		Smalltalk allClassesDo: [ :cls |
			cls releaseClassState ]].
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [
		Transcript clear.
		"Enable this call to actually see the image clean report."
		false ifTrue: [Smalltalk printStuffToCleanOnImageSave]].
	"Do image save & quit as apropriate"
	(Cursor cursorAt: #writeCursor) activateCursor.
	save
		ifTrue: [
			"The snapshot primitive answers false if it was just called to do the snapshot.
			But image startup is resumed by returning (again) from the primitive, but this time answering true."
			isARealStartup := embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ isARealStartup := false ].
	quit & (isARealStartup == false) ifTrue: [ self quitPrimitive ].

	"If starting from absolute scratch, this would be a good time to recreate Global names"
	Smalltalk at: #Sensor put: nil.
	Smalltalk at: #SourceFiles put: (Array new: 2).
	self logStartupDebugAid: 'To #openSourceFiles'.
	Smalltalk openSourceFiles.
	"Here, startup begins!! (isARealStartup might be nil)"
	self logStartupDebugAid: 'To #initClassCachedState'.
	Smalltalk allClassesDo: [ :cls | cls initClassCachedState ].
	self logStartupDebugAid: 'To #doStartUp:'.
	self doStartUp: isARealStartup == true.
	self logStartupDebugAid: 'To #spawnNewMorphicProcessFor: ', guiRootObject printString.
	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject := guiRootObjectClass newWorld ]).
	self logStartupDebugAid: 'To #restoreLostChangesIfNecessary'.
	self restoreLostChangesIfNecessary.
	clearAllStateFlag ifTrue: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #recreateDefaultDesktop'.
			guiRootObject recreateDefaultDesktop; restoreDisplay ]]
	ifFalse: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #restoreDisplay'.
			guiRootObject restoreDisplay ]].
	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #readAndApplyUserPrefs'.
			self readAndApplyUserPrefs.
			self logStartupDebugAid: 'To #processCommandLineArguments'.
			self processCommandLineArguments.
			self logStartupDebugAid: 'To #launchApp'.
			AppLauncher launchApp ]].
	"Now it's time to raise an error"
	isARealStartup ifNil: [ self logStartupError: 'Failed to write image file (disk full?)' ]
]
	forkAt: Processor timingPriority - 1
	named: 'Startup process'.! !


!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 12/11/2023 16:14:58'!
beDisplay
	"Tell the interpreter to use the receiver as the current display image.
	Essential.  See Object documentation whatIsAPrimitive.

	Note: Some VMs (in particular MacOS OpenSmalltalk VM as of 2023, but maybe others) will pin our bits,
	store the pointer in the VM and use it at will (for instance for updating OS Display).
	This means that Smalltalk must hold a strong reference to 'bits' until a new one is stored in a later call
	to this primitive.
	See senders (usually only one), and the use of the CurrentDisplayBits class variable.
	Additionally note that it is useless to nil the Display global variable. The CurrentDisplayBits class variable will still point to the bits!!
	See http://lists.squeakfoundation.org/archives/list/vm-dev@lists.squeakfoundation.org/message/F3I7L5IJT2J5RXCS3QXLLQ3TZ6KU6FD6/"

	<primitive: 102>
	self primitiveFailed.! !

!DisplayScreen methodsFor: 'private' stamp: 'jmv 12/11/2023 16:28:23'!
setExtent: aPoint depth: bitsPerPixel
	"DisplayScreen startUp"
	"This method is critical. If the setExtent fails, there will be no
	proper display on which to show the error condition."

	| bitsPerPixelToUse |
	(depth = bitsPerPixel and: [aPoint = self extent and: [
					self supportsDisplayDepth: bitsPerPixel]]) ifFalse: [
		bitsPerPixelToUse := (self supportsDisplayDepth: bitsPerPixel)
			ifTrue: [ bitsPerPixel ]
			ifFalse: [
				(self supportsDisplayDepth: bitsPerPixel negated)
					ifTrue: [ bitsPerPixel negated ]
					ifFalse: [ self findAnyDisplayDepth ]].
		"This super call will assign the new contents of the 'bits' instance variable.
		The old one may be still used by the VM (see comment at #beDisplay),
		but it is also stored in the CurrentDisplayBits class variable, so it is safe."
		super setExtent: aPoint depth: bitsPerPixelToUse.

		"Usually you don't create new instances of DisplayScreen, but if ever done, do what follows only for the real Display."
		self == Display ifTrue: [
			"It is important to keep CurrentDisplayBits referencing the bits passed to the VM in the previous
			call to #beDisplay until AFTER we passed it the new value in the call that follows."
			self beDisplay.
			"Only after that it is save to store the new bits, so there is a strong reference to it until the next
			time #beDisplay is called."
			CurrentDisplayBits := bits.
		].
	].! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 12/11/2023 16:54:59'!
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
	Display setExtent: self actualScreenSize depth: Display nativeDepth.
	self installScreenUpdater.! !


!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 12/11/2023 16:10:52'!
pvtSetForm: aForm

	| s |
	"Unpin if pinning is supported by the VM, NOP otherwise."
	targetForm ifNotNil: [ targetForm bits unpin ].
	morphIds ifNotNil: [ morphIds unpin ].
	edgeCounts ifNotNil: [ edgeCounts unpin ].
	alphaMask ifNotNil: [ alphaMask unpin ].
	contour ifNotNil: [ contour unpin ].

	targetForm := aForm.
	targetWidth := targetForm width.
	targetHeight := targetForm height.
	s := targetWidth * targetHeight.
	self flag: #VEPapi5.		"Eventually remove the +1.
	Needed for VEPapi=5 and 6 (for plugins older than Sept-21-2021, and version less than 1.29).
	Avoid it altoghether if the plugin is not available (#pluginApiVersion answers 0)"
	morphIds := WordArray new: (
		(VectorEngineWithPlugin pluginApiVersion between: 5 and: 6)
			ifTrue: [s+1]
			ifFalse: [s]).

	edgeCounts := self auxPixelDataClass new: s.
	alphaMask := self auxPixelDataClass new: s.
	contour := Float32Array new: targetHeight * 2.

	"Pin if pinning is supported by the VM, NOP otherwise."
	"Pinning these large arrays should relieve some burden on the GC.
	Additionally, VectorEngineWithPlugin>>#protected: ensures that while drawing a path no one else will
	use the plugin until path is finished. This means that, on Spur with pinned arrays, arguments
		targetBits, targetWidth, targetHeight, morphIds, edgeCounts, alphaMask, contour
	can be removed from most calls. (see #initializePath)
	In order to remove them from any calls, it would be needed to ensure that the VM supports pinning!!"
	"Warning: #pin may fail for Spur images if run on the SqueakJS VM, at least on October 2023."
	targetForm bits pin. morphIds pin. edgeCounts pin. alphaMask pin. contour pin.

	0 to: targetHeight-1 do: [ :y |
		contour at: y * 2 + 1 put: targetWidth.
		contour at: y * 2 + 2 put: 0 ].
	self resetMorphBounds.
	self clipRect: targetForm boundingBox.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.
	geometryTransformation := MorphicTranslation identity.! !

!classDefinition: #DisplayScreen category: #'Graphics-Display Objects'!
Form subclass: #DisplayScreen
	instanceVariableNames: ''
	classVariableNames: 'CurrentDisplayBits Damage ScreenUpdaterProcess ScreenUpdaterSemaphore'
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

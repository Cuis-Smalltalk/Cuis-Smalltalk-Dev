'From Cuis7.9 [latest update: #8278] on 23 October 2026 at 10:13:51 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 23/Oct/2026, 1:13:43 pm (UTC)'!
snapshot: save andQuit: quit withExitCode: exitCode
	"Alternative method for image save and / or quit.
	This method is useful for creating images that resume an ongoing process, for instance
	for debugging purposes.
	It is also close to the related methods in Squeak, and different from
		#snapshot:andQuit:embedded:clearAllClassState:
	that is meant for saving 'clean', general purpose images."
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	If exitCode is not nil, then use it as exit code.
	The latter part of this method runs when resuming a previously saved image."
	"
	To test, evaluate: (warning, this saves the image and exits!!)
		{DateAndTime now. 'before toto'} print.
		Smalltalk snapshot: true andQuit: true withExitCode: 42.
		{DateAndTime now. 'after toto'} print.
	"

	| isARealStartup |
	"If it is just about quitting, do it the simpler way."
	save not & quit ifTrue: [
		self quit.
		"Execution doesn't reach this point"
		^self ].

	self logSnapshot: save andQuit: quit.
	
	"Be sure all unreferenced objects are finalized as approriate.
	Do this again later, as close to the snapshot / quit primitive as possible."
	self garbageCollect.
	EphemeronFinalizationProcess ensureFinalizationComplete.
	EphemeronFinalizationProcess stopFinalizationProcess.

	self processShutDownList: quit.	
	self closeSourceFiles.
	(Cursor cursorAt: #writeCursor) activateCursor.
	"The snapshot primitive will run a GC, but any detected Ephemeron will not be finalized, and the finalization
	queue will be cleared by the VM both in running and snapshotted image. This means they will never be finalized!!
	To avoid that, just before snapshot, do a GC.
	We also need to do #mournAllPending because TheFinalizationProcess has been already stopped.
	Repeat this until no further Ephemerons are to be processed."
	[
		self garbageCollect.
		EphemeronFinalizationProcess mournAllPending ] whileFalse.

	"Do image save & quit as apropriate"
	isARealStartup := false.
	save ifTrue: [
		"The snapshot primitive answers false if it was just called to do the snapshot.
		But image startup is resumed by returning (again) from the primitive, but this time answering true.
		nil means saving image failed"
		isARealStartup := self snapshotPrimitive ].
	
	quit & (isARealStartup == false) ifTrue: [
		exitCode
			ifNil: [ self quitPrimitive ]
			ifNotNil: [ self quitPrimitive: exitCode ]
		]. "Don't quit on nil (image save failed)"

	isARealStartup == true
		ifTrue: [ DoingStartup := true ].

	"Here, startup begins!! (isARealStartup might be nil)"
	self doStartUp: isARealStartup == true.
	self openSourceFiles.
	self processStartUpList: isARealStartup.
	isARealStartup == true
		ifTrue: [ self setStartupStamp ].
	isARealStartup == true
		ifTrue: [ self assureStartupStampLogged ].
	"It is needed to do this at least once on MacVMs, where it may not be needed later.
	For instance #forceToScreen: is usually not called in #displayWorldOn: if running on Mac."
	Display forceToScreen.
	DoingStartup := false.! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 23/Oct/2026, 12:53:54 pm (UTC)'!
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
	"If it is just about quitting, do it the simpler way."
	save not & quit ifTrue: [
		self quit.
		"Execution doesn't reach this point"
		^self ].
	
	self logSnapshot: save andQuit: quit.
	clearAllStateFlag ifTrue: [
		TranscriptWindow allInstancesDo: [ :each |
			each isInWorld ifTrue: [ each delete.]].
		UISupervisor ui tearDownDesktop.
		Transcript logToFile: false ].
	"These could be moved to some #shutDown"
	self stopLowSpaceWatcher.
	ProcessorScheduler stopBackgroundProcess.
	"Be sure all unreferenced objects are finalized as approriate.
	Do this again later, as close to the snapshot / quit primitive as possible."
	self garbageCollect.
	EphemeronFinalizationProcess ensureFinalizationComplete.
	EphemeronFinalizationProcess stopFinalizationProcess.
	activeProc := Processor activeProcess.
[
	| isARealStartup guiRootObject guiRootObjectClass userBaseDirectory |
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
		c nilState ].
	self processShutDownList: quit.

	"Clean Globals"
	userBaseDirectory := DirectoryEntry userBaseDirectory.
	self at: #Sensor put: nil.
	self closeSourceFiles.
	self at: #SourceFiles put: nil.
	self allClassesDo: [ :cls |
		cls releaseClassCachedState ].
	clearAllStateFlag ifTrue: [
		self allClassesDo: [ :cls |
			cls releaseClassState ].
		"Usually we set it during startup, in #readAndApplyUserPrefs"
		Utilities clearAuthor ].
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [
		Transcript clear.
		"Enable this call to actually see the image clean report."
		false ifTrue: [self printStuffToCleanOnImageSave]].
	(Cursor cursorAt: #writeCursor) activateCursor.
	"The snapshot primitive will run a GC, but any detected Ephemeron will not be finalized, and the finalization
	queue will be cleared by the VM both in running and snapshotted image. This means they will never be finalized.
	To avoid that, just before snapshot, do a GC.
	We also need to do #mournAllPending because TheFinalizationProcess has been already stopped.
	Repeat this until no further Ephemerons are to be processed."
	[
		self garbageCollect.
		EphemeronFinalizationProcess mournAllPending ] whileFalse.
	"Do image save & quit as apropriate"
	isARealStartup := false.
	save ifTrue: [
		"The snapshot primitive answers false if it was just called to do the snapshot.
		But image startup is resumed by returning (again) from the primitive, but this time answering true.
		nil means saving image failed."
		isARealStartup := embeddedFlag
			ifTrue: [ self snapshotEmbeddedPrimitive ]
			ifFalse: [ self snapshotPrimitive ]].
	quit & (isARealStartup == false) ifTrue: [
		self quitPrimitive ]. "Don't quit on nil (image save failed)"

	isARealStartup == true
		ifTrue: [
			DoingStartup := true ]
		ifFalse: [
			"Keep current #userBaseDirectory unless a real startup"
			DirectoryEntry setUserBaseDirectory: userBaseDirectory ].

	"Here, startup begins!! (isARealStartup might be nil)"
	self logStartupDebugAid: 'To #initClassCachedState'.
	self allClassesDo: [ :cls | cls initClassCachedState ].
	self logStartupDebugAid: 'To #doStartUp:'.
	self doStartUp: isARealStartup == true.
	"Some command line arguments need to be processed before starting the UI"
	self readCommandLineArguments.
	isARealStartup == true ifTrue: [
		self processCommandLineArguments: true ].
	"Recreate Global names"
	self at: #Sensor put: nil.
	self at: #SourceFiles put: (Array new: 2).
	isARealStartup == true
		ifTrue: [
			self logStartupDebugAid: 'To #readAndApplyUserPrefs'.
			self readAndApplyUserPrefs ].
	self logStartupDebugAid: 'To #openSourceFiles'.
	self openSourceFiles.
	self processStartUpList: isARealStartup.
	isARealStartup == true ifTrue: [
		self setStartupStamp ].
	self logStartupDebugAid: 'To #spawnNewMorphicProcessFor: ', guiRootObject printString.
	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject := guiRootObjectClass newWorld ]).
	isARealStartup == true
		ifTrue: [
			self logStartupDebugAid: 'To #checkIfAlreadyRunningOrStoppedNoExit'.
			self checkIfAlreadyRunningOrStoppedNoExit.
			self assureStartupStampLogged ].
	clearAllStateFlag ifTrue: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #recreateDefaultDesktop'.
			guiRootObject recreateDefaultDesktop; restoreDisplay ]]
	ifFalse: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #restoreDisplay'.
			guiRootObject restoreDisplay ]].
	"It is needed to do this at least once on MacVMs, where it may not be needed later.
	For instance #forceToScreen: is usually not called in #displayWorldOn: if running on Mac."
	Display forceToScreen.
	isARealStartup == true
		ifTrue: [
			"If system is coming up (VM and image just started)"
			UISupervisor whenUIinSafeState: [
				self logStartupDebugAid: 'To #processCommandLineArguments'.
				"Some command line arguments need to be processed after the UI is up, and in the UI process"
				self processCommandLineArguments: false.
				DoingStartup := false.
				self logStartupDebugAid: 'To #launchApp'.
				AppLauncher launchApp ]]
		ifFalse: [
			"If we are just saving the image"
			UISupervisor whenUIinSafeState: [
				self logStartupDebugAid: 'To #readAndApplyUserPrefs'.
				self readAndApplyUserPrefs ]].
	"Now it's time to raise an error"
	isARealStartup ifNil: [ self logStartupError: 'Failed to write image file (disk full?)' ]
]
	forkAt: Processor timingPriority - 1
	named: 'Startup process'.
! !


!EphemeronFinalizationProcess class methodsFor: 'accessing' stamp: 'jmv 22/Oct/2026, 2:13:00 pm (UTC)'!
mournAllPending
	"See #finalizationProcess.
	This additional implementation is only to be called one last time before snapshot / quit.
	Warning: Unlike #finalizationProcess, this run is not protected against failure in #mourn.
		Failure in #mourn will halt snapshot / quit.
	Return whether no ephemerons were mourned."

	^self primitiveFetchMourner
		ifNotNil: [ :firstMourner |
			self mournLoopWith: firstMourner.
			false ]
		ifNil: [ true ]! !


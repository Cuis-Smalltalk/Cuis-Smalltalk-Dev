'From Cuis7.1 [latest update: #6819] on 18 November 2024 at 7:03:17 pm'!

!WeakArray class methodsFor: 'services' stamp: 'eem 11/18/2024 18:51:12'!
ensureFinalizationComplete

	Smalltalk garbageCollect.
	Processor activePriority >= FinalizationProcess priority ifTrue: [
		"Not needed if the default priorities are used. Just in case."
		Processor activeProcess priority: FinalizationProcess priority-1.
		Processor yield ].! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 11/18/2024 18:10:19'!
quit
	"Just quit. No questions asked. No validations done.
	Smalltalk quit.
	"

	(SourceFiles at: 2) ifNotNil: [ :changes |
		ChangesInitialFileSize ifNotNil: [ changes truncate: ChangesInitialFileSize ]].
	ActiveModel flushEventSystem.
	"These could be moved to some #shutDown"
	self stopLowSpaceWatcher.
	ProcessorScheduler stopBackgroundProcess.

	"Be sure all unreferenced objects are finalized as approriate."
	WeakArray ensureFinalizationComplete.
	WeakArray stopFinalizationProcess.

	self processShutDownList: true.
	self closeSourceFiles.

	"Do image save & quit as apropriate"
	self quitPrimitive.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 11/18/2024 19:01:28'!
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
		^self.
	].
	self logSnapshot: save andQuit: quit.
	clearAllStateFlag ifTrue: [
		TranscriptWindow allInstancesDo: [ :each |
			each isInWorld ifTrue: [ each delete.]].
		UISupervisor ui tearDownDesktop.
		Transcript logToFile: false ].
	ActiveModel flushEventSystem.
	"These could be moved to some #shutDown"
	self stopLowSpaceWatcher.
	ProcessorScheduler stopBackgroundProcess.
	"Be sure all unreferenced objects are finalized as approriate."
	WeakArray ensureFinalizationComplete.
	WeakArray stopFinalizationProcess.
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
			cls releaseClassState ]].
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [
		Transcript clear.
		"Enable this call to actually see the image clean report."
		false ifTrue: [self printStuffToCleanOnImageSave]].
	"Do image save & quit as apropriate"
	(Cursor cursorAt: #writeCursor) activateCursor.
	isARealStartup := false.
	save ifTrue: [
		"The snapshot primitive answers false if it was just called to do the snapshot.
		But image startup is resumed by returning (again) from the primitive, but this time answering true.
		nil means saving image failed"
		isARealStartup := embeddedFlag
			ifTrue: [ self snapshotEmbeddedPrimitive ]
			ifFalse: [ self snapshotPrimitive ]].
	quit & (isARealStartup == false) ifTrue: [ self quitPrimitive ]. "Don't quit on nil (image save failed)"

	isARealStartup == true ifFalse: [ 									"Keep current #userBaseDirectory unless a real startup"
		DirectoryEntry setUserBaseDirectory: userBaseDirectory ].

	"If starting from absolute scratch, this would be a good time to recreate Global names"
	self at: #Sensor put: nil.
	self at: #SourceFiles put: (Array new: 2).
	self logStartupDebugAid: 'To #openSourceFiles'.
	self openSourceFiles.
	"Here, startup begins!! (isARealStartup might be nil)"
	self logStartupDebugAid: 'To #initClassCachedState'.
	self allClassesDo: [ :cls | cls initClassCachedState ].
	self logStartupDebugAid: 'To #doStartUp:'.
	self doStartUp: isARealStartup == true.
	self logStartupDebugAid: 'To #spawnNewMorphicProcessFor: ', guiRootObject printString.
	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject := guiRootObjectClass newWorld ]).
	isARealStartup == true
		ifTrue: [
			self logStartupDebugAid: 'To #checkIfAlreadyRunningOrStoppedNoExit'.
			self checkIfAlreadyRunningOrStoppedNoExit.
			"Some command line arguments need to be processed before starting the UI"
			self processCommandLineArguments: true.
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
				self logStartupDebugAid: 'To #readAndApplyUserPrefs'.
				self readAndApplyUserPrefs.
				self logStartupDebugAid: 'To #processCommandLineArguments'.
				"Some command line arguments need to be processed after the UI is up, and in the UI process"
				self processCommandLineArguments: false.
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
	named: 'Startup process'.! !


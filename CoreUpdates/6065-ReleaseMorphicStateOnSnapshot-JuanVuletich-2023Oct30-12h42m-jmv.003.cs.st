'From Cuis 6.0 [latest update: #6064] on 30 October 2023 at 1:02:13 pm'!

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 10/30/2023 13:01:51'!
releaseEngine
	engine := nil.
	ownerProcess := nil.
	world := nil.
	form := nil.! !


!VectorCanvas methodsFor: 'private' stamp: 'jmv 10/30/2023 12:56:29'!
releaseEngine
	super releaseEngine.
	boundsFinderCanvas releaseEngine.
	auxBitBltEngine := nil.! !


!BitBltCanvas methodsFor: 'private' stamp: 'jmv 10/30/2023 12:56:36'!
releaseEngine
	super releaseEngine.
	boundsFinderCanvas releaseEngine.! !


!HybridCanvas methodsFor: 'private' stamp: 'jmv 10/30/2023 12:56:40'!
releaseEngine
	super releaseEngine.
	morphIDsForm := nil.! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 10/30/2023 13:02:00'!
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
	Smalltalk at: #Display put: nil.
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
	Smalltalk at: #Display put: DisplayScreen new.
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


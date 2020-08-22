'From Cuis 5.0 [latest update: #4354] on 21 August 2020 at 7:28:31 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/21/2020 19:28:21'!
snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: clearAllStateFlag
	"WARNING: Current process will be killed.
UI Process will be restarted
"
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."
	"
	To test the full cleanup and startup procedures, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: true

	To test the cleanup done when saving the image, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: false
	"
	| activeProc |
	activeProc _ Processor activeProcess.
	[ | isARealStartup guiRootObject guiRootObjectClass |
	save not & quit
		ifTrue: [
			(SourceFiles at: 2) ifNotNil: [ :changes |
				ChangesInitialFileSize ifNotNil: [ changes truncate: ChangesInitialFileSize ]]]
		ifFalse: [
			self
				logSnapshot: save
				andQuit: quit ].
	clearAllStateFlag ifTrue: [
		TranscriptWindow allInstancesDo: [ :each |
			each isInWorld ifTrue: [
				each delete.]].
		UISupervisor ui tearDownDesktop.
		Transcript logToFile: false ].
	ActiveModel flushEventSystem.
	self processShutDownList: quit.
	"Lo que sigue aca podria ir al shutdown de alguien... (se levantan en startup!!)"
	Smalltalk stopLowSpaceWatcher.
	WeakArray stopFinalizationProcess.
	ProcessorScheduler stopBackgroundProcess.
	"Cosas que levanto explicitamente abajo"
	guiRootObjectClass _ UISupervisor ui class.
	guiRootObject _ UISupervisor ui.
	"Replace with this to create a new world at startup after 'saveAsNewVersion'"
	"guiRootObject _ clearAllStateFlag ifFalse: [ UISupervisor ui ]."
	UISupervisor stopUIProcess.
	activeProc isTerminated ifFalse: [ activeProc terminate ].
	"Clean Globals"
	Smalltalk
		at: #Sensor
		put: nil.
	Smalltalk
		at: #Display
		put: nil.
	Smalltalk closeSourceFiles.
	Smalltalk
		at: #SourceFiles
		put: nil.
	Smalltalk allClassesDo: [ :cls |
		cls releaseClassCachedState ].
	clearAllStateFlag ifTrue: [
		Smalltalk allClassesDo: [ :cls |
			cls releaseClassState ]].
	"Ojo con los pool dicts. Creo que no hay ninguno..."
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [
		Smalltalk printStuffToCleanOnImageSave.
		"Remove this call to actually see the image clean report."
		Transcript clear.
		].
	"Do image save & quit as apropriate"
	(Cursor cursorAt: #writeCursor) activateCursor.
	save
		ifTrue: [
			"The snapshot primitive answers false if it was just called to do the snapshot.
			But image startup is resumed by returning (again) from the primitive, but this time answering true."
			isARealStartup _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ isARealStartup _ false ].
	quit & (isARealStartup == false) ifTrue: [ self quitPrimitive ].
	"If starting from absolute scratch, this would be a good time to recreate Global names"
	Smalltalk
		at: #Sensor
		put: nil.
	Smalltalk
		at: #Display
		put: DisplayScreen new.
	Smalltalk
		at: #SourceFiles
		put: (Array new: 2).
	Smalltalk openSourceFiles.
	"Here, startup begins!! (isARealStartup might be nil)"
	Smalltalk allClassesDo: [ :cls |
		cls initClassCachedState ].
	self doStartUp: isARealStartup == true.
	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject _ guiRootObjectClass newWorld ]).
	self restoreLostChangesIfNecessary.
	clearAllStateFlag ifTrue: [
		UISupervisor whenUIinSafeState: [
			guiRootObject recreateDefaultDesktop; restoreDisplay.
			Display triggerEvent: #screenSizeChanged ]]
	ifFalse: [
		UISupervisor whenUIinSafeState: [
			guiRootObject restoreDisplay.
			Display triggerEvent: #screenSizeChanged ]].
	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [
		UISupervisor whenUIinSafeState: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].
	"Now it's time to raise an error"
	isARealStartup ifNil: [ self error: 'Failed to write image file (disk full?)' ]]
		forkAt: Processor timingPriority - 1
		named: 'Startup process'.! !


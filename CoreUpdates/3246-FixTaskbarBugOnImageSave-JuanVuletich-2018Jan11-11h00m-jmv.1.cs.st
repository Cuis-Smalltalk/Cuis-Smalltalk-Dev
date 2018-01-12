'From Cuis 5.0 of 7 November 2016 [latest update: #3245] on 11 January 2018 at 11:01:17 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/11/2018 00:15:12'!
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
[ 
	| isARealStartup guiRootObject guiRootObjectClass reopenTranscript |
	self logSnapshot: save andQuit: quit.

	reopenTranscript _ false.
	clearAllStateFlag ifTrue: [
		TranscriptWindow allInstancesDo: [ :each | each isInWorld ifTrue: [ each delete. reopenTranscript _ true ]].
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
	activeProc isTerminated ifFalse: [
		activeProc terminate ].

	"Clean Globals"
	Smalltalk at: #Sensor put: nil.
	Smalltalk at: #Display put: nil.
	Smalltalk closeSourceFiles. Smalltalk at: #SourceFiles put: nil.
	Smalltalk allClassesDo: [ :cls | cls releaseClassCachedState ].
	clearAllStateFlag ifTrue: [	
		Smalltalk allClassesDo: [ :cls | cls releaseClassState ]].
	"Ojo con los pool dicts. Creo que no hay ninguno..."
	
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [ Smalltalk printStuffToCleanOnImageSave ].


	"Do image save & quit as apropriate"
	Cursor write activateCursor.
	save
		ifTrue: [
			isARealStartup _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ isARealStartup _ false ].
	quit & (isARealStartup == false) ifTrue: [ self quitPrimitive ].


	"If starting from absolute scratch, this would be a good time to recreate Global names"
	Smalltalk at: #Sensor put: nil.
	Smalltalk at: #Display put: (DisplayScreen extent: `1024@768` depth: 32).
	Smalltalk at: #SourceFiles put: (Array new: 2).
	Smalltalk openSourceFiles.

	"Here, startup begins!! (isARealStartup might be nil)"
	Smalltalk allClassesDo: [ :cls | cls initClassCachedState ].
	self doStartUp: isARealStartup == true.

	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject _ guiRootObjectClass newWorld ]).
	Display triggerEvent: #screenSizeChanged.

	reopenTranscript ifTrue: [
		UISupervisor whenUIinSafeState: [
			TranscriptWindow openTranscript ]].
	"
	UISupervisor whenUIinSafeState: [
		guiRootObject fullRepaintNeeded ].
	"

	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [
		UISupervisor whenUIinSafeState: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].

	"Now it's time to raise an error"
	isARealStartup ifNil: [ self error: 'Failed to write image file (disk full?)' ].

] forkAt: Processor timingPriority-1 named: 'Startup process'.! !


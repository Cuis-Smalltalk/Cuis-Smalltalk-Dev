'From Cuis 4.2 of 25 July 2013 [latest update: #2650] on 20 December 2015 at 1:02:06.184701 am'!

!PasteUpMorph class methodsFor: 'instance creation' stamp: 'jmv 12/20/2015 00:39'!
newWorld
	"
[
	ProjectX stopUIProcess.
	ProjectX spawnNewMorphicProcessFor: PasteUpMorph newWorld
] fork.
	"
	| w ws |
	w _ self new.
	ws _ WorldState new.
	w worldState: ws.
	w viewBox: Display boundingBox.    "do first since it may clear canvas"
	ws setCanvas: Display getCanvas.
	w borderWidth: 0.
	ws handsDo: [ :h |
		h privateOwner: w ].
	^w! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/20/2015 01:01'!
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
	| resuming world reopenTranscript |
	self logSapshot: save andQuit: quit.

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
	world _ ProjectX ui.
	"Replace with this to create a new world at startup after 'saveAsNewVersion'"
	"world _ clearAllStateFlag ifFalse: [ ProjectX ui ]."
	ProjectX stopUIProcess.
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
			resuming _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ resuming _ false ].
	quit & (resuming == false) ifTrue: [ self quitPrimitive ].


	"If starting from absolute scratch, this would be a good time to recreate Global names"
	Smalltalk at: #Sensor put: nil.
	Smalltalk at: #Display put: (DisplayScreen extent: 1024@768 depth: 32).
	Smalltalk at: #SourceFiles put: (Array new: 2).
	Smalltalk openSourceFiles.

	"Here, startup begins!! (resuming might be nil)"
	Smalltalk allClassesDo: [ :cls | cls initClassCachedState ].
	self startup: resuming == true.
	
	
	ProjectX spawnNewMorphicProcessFor: (world ifNil: [ PasteUpMorph newWorld ]).

	reopenTranscript ifTrue: [
		WorldState addDeferredUIMessage: [
			TranscriptWindow openTranscript ]].
	"
	WorldState addDeferredUIMessage: [
		world fullRepaintNeeded ].
	"

	"If system is coming up (VM and image just started)"
	resuming == true ifTrue: [	
		WorldState addDeferredUIMessage: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].

	"Now it's time to raise an error"
	resuming ifNil: [ self error: 'Failed to write image file (disk full?)' ].

] forkAt: Processor timingPriority-1 named: 'Startup process'.! !


!WorldState methodsFor: 'initialization' stamp: 'jmv 12/20/2015 00:28'!
initialize

	activeHand _ HandMorph new.
	hands _ { activeHand }.
	damageRecorder _ DamageRecorder new.
	stepList _ Heap sortBlock: self stepListSortBlock.
	lastAlarmTime _ 0.
	drawingFailingMorphs _ WeakIdentitySet new.
	pause _ 20.
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ false! !


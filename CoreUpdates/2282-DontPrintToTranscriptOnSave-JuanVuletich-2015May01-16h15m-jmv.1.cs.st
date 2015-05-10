'From Cuis 4.2 of 25 July 2013 [latest update: #2280] on 1 May 2015 at 4:15:40.273889 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/1/2015 16:15'!
snapshot: save andQuit: quit embedded: embeddedFlag
"WARNING: Current process will be killed.
UI Process will be restarted
"
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."
| activeProc |
activeProc _ Processor activeProcess.
[ 
	| resuming world |
	self logSapshot: save andQuit: quit.
	TranscriptWindow allInstancesDo: [ :each | each delete ].
	Transcript logToFile: false.
	ActiveModel flushEventSystem.
	self processShutDownList: quit.


	"Lo que sigue aca podria ir al shutdown de alguien... (se levantan en startup!!)"
	Smalltalk stopLowSpaceWatcher.
	WeakArray stopFinalizationProcess.
	ProcessorScheduler stopBackgroundProcess.
	world _ ProjectX ui.
	ProjectX stopUIProcess.
	activeProc isTerminated ifFalse: [
		activeProc terminate ].

	"Clean Globals"
	Smalltalk at: #Sensor put: nil.
	Smalltalk at: #Display put: nil.
	Smalltalk closeSourceFiles. Smalltalk at: #SourceFiles put: nil.
	Smalltalk allClassesDo: [ :cls | cls releaseClassCachedState ].
	"Ojo con los pool dicts. Creo que no hay ninguno..."
	
	"To keep cleaning stuff that shouldn't be saved..."
	false ifTrue: [ Smalltalk printStuffToCleanOnImageSave ].


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
	Smalltalk at: #SourceFiles put: (Array new: 2).	Smalltalk openSourceFiles.

	"Here, startup begins!! (resuming might be nil)"
	self startup: resuming == true.
	
	
	ProjectX spawnNewMorphicProcessFor: world.
	
	WorldState addDeferredUIMessage: [
		TranscriptWindow openTranscript ].

	"Now it's time to raise an error"
	resuming ifNil: [ self error: 'Failed to write image file (disk full?)' ].

] forkAt: Processor timingPriority-1 named: 'Startup process'.! !


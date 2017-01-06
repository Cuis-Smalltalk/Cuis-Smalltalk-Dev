'From Cuis 5.0 of 7 November 2016 [latest update: #3024] on 6 January 2017 at 10:05:27 am'!

!WeakArray class methodsFor: 'system startup' stamp: 'jmv 1/6/2017 09:56:14'!
startUp
	"Do it even if just continuing after image snapshot"
	self restartFinalizationProcess! !


!SystemDictionary methodsFor: 'startup' stamp: 'jmv 1/6/2017 09:59:32'!
doStartUp: isARealStartup
	"
	isARealStartup
		true: system is coming up (VM and image just started)
		false: we have just saved an image snapshot, but didn't quit.
	"

	"Here, startup begins!!"
	Cursor normal activateCursor.
	self setGCParameters.
	isARealStartup ifTrue: [
		self clearExternalObjects ].
	self readCommandLineArguments.
	self processStartUpList: isARealStartup.
	isARealStartup ifTrue: [
		self setPlatformPreferences.
		self setStartupStamp ]! !


!Behavior methodsFor: 'system startup' stamp: 'jmv 1/6/2017 09:52:14'!
startUp
	"This message is sent to registered classes when the system is coming up, or after an image save."! !

!Behavior methodsFor: 'system startup' stamp: 'jmv 1/6/2017 09:53:18'!
startUp: isARealStartup
	"This message is sent to registered classes, with isARealStartup = true when the system is coming up,
	and with isARealStartup = false after a snapshot (image save, no quit).
	Classes caring about the difference should reimplement this method."

	^ self startUp! !


!WeakArray class methodsFor: 'class initialization' stamp: 'jmv 1/6/2017 09:49:16'!
initialize
	"
	WeakArray initialize.
	SystemDictionary initialize.
	"

	self restartFinalizationProcess! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/6/2017 09:53:30'!
processStartUpList: isARealStartup
	"Send #startUp to each class that needs to run initialization after a snapshot."

	EndianCache _ self calcEndianness.
	self send: #startUp: toClassesNamedIn: StartUpList with: isARealStartup! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/6/2017 09:55:41'!
send: startUpOrShutDown toClassesNamedIn: startUpOrShutDownList with: argument
	"Send the message #startUp: or #shutDown: to each class named in the list.
	The argument indicates if the system is about to quit (for #shutDown:) or if
	the image is ia real startup (or just continue after image save) (for #startUp:).
	If any name cannot be found, then remove it from the list."

	| removals class |
	removals _ OrderedCollection new.
	startUpOrShutDownList do:
		[:name |
		class _ self at: name ifAbsent: nil.
		class
			ifNil: [removals add: name]
			ifNotNil: [
				class isInMemory ifTrue: [
					class perform: startUpOrShutDown with: argument]]].

	"Remove any obsolete entries, but after the iteration"
	"Well, not. Better just ignore them. Maybe it is stuff, like SoundPlayer, that was moved to optional packages, and can be loaded again anytime."
	"startUpOrShutDownList removeAll: removals"! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/6/2017 09:59:34'!
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
	| isARealStartup world reopenTranscript |
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
			isARealStartup _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ isARealStartup _ false ].
	quit & (isARealStartup == false) ifTrue: [ self quitPrimitive ].


	"If starting from absolute scratch, this would be a good time to recreate Global names"
	Smalltalk at: #Sensor put: nil.
	Smalltalk at: #Display put: (DisplayScreen extent: 1024@768 depth: 32).
	Smalltalk at: #SourceFiles put: (Array new: 2).
	Smalltalk openSourceFiles.

	"Here, startup begins!! (isARealStartup might be nil)"
	Smalltalk allClassesDo: [ :cls | cls initClassCachedState ].
	self doStartUp: isARealStartup == true.
	
	
	ProjectX spawnNewMorphicProcessFor: (world ifNil: [ PasteUpMorph newWorld ]).

	reopenTranscript ifTrue: [
		WorldState addDeferredUIMessage: [
			TranscriptWindow openTranscript ]].
	"
	WorldState addDeferredUIMessage: [
		world fullRepaintNeeded ].
	"

	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [	
		WorldState addDeferredUIMessage: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].

	"Now it's time to raise an error"
	isARealStartup ifNil: [ self error: 'Failed to write image file (disk full?)' ].

] forkAt: Processor timingPriority-1 named: 'Startup process'.! !

!methodRemoval: SystemDictionary #startup:!
SystemDictionary removeSelector: #startup:!
!methodRemoval: WeakArray class #startUp:!
WeakArray class removeSelector: #startUp:!
WeakArray initialize!

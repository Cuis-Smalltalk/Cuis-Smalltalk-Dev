'From Cuis 5.0 of 7 November 2016 [latest update: #3253] on 15 January 2018 at 4:47:07 pm'!

!DisplayScreen methodsFor: 'initialization' stamp: 'jmv 1/15/2018 15:51:20'!
initialize
	self
		setExtent: self class actualScreenSize
		depth: (self class actualScreenDepth ifNil: [ 32 ])! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 1/15/2018 16:08:16'!
getPreambleFrom: aFileStream at: position
	|  writeStream c p |
	writeStream _ String new writeStream.
	p _ position.
	c _ nil.
	[ p >= 0 and: [ c ~~ $!! ]] whileTrue: [
		aFileStream position: p.
		c _ aFileStream basicNext.
		p _ p - 1 ].
	[ p >= 0] whileTrue: [
		aFileStream position: p.
		c _ aFileStream basicNext.
		c == $!!
			ifTrue: [^ writeStream contents reverse ]
			ifFalse: [ writeStream nextPut: c ].
		p _ p - 1 ].
	^ nil! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 1/15/2018 16:08:53'!
timeStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	"(CompiledMethod compiledMethodAt: #timeStamp) timeStamp"

	| file preamble stamp tokens tokenCount |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file _ SourceFiles at: self fileIndex.
	file ifNil: [^ String new].  "sources file not available"
	"file does not exist happens in secure mode"
	file _ [file name asFileEntry readStream] on: FileDoesNotExistException do: [ :ex| nil ].
	file ifNil: [^ String new].
	preamble _ self getPreambleFrom: file at: (0 max: self filePosition).
	preamble ifNil: [ ^ '' ].
	stamp _ String new.
	tokens _ (preamble findString: 'methodsFor:' startingAt: 1) > 0
		ifTrue: [Scanner new scanTokens: preamble]
		ifFalse: [Array new  "ie cant be back ref"].
	(((tokenCount _ tokens size) between: 7 and: 8) and: [(tokens at: tokenCount - 5) = #methodsFor:])
		ifTrue:
			[(tokens at: tokenCount - 3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp _ tokens at: tokenCount - 2]].
	((tokenCount between: 5 and: 6) and: [(tokens at: tokenCount - 3) = #methodsFor:])
		ifTrue:
			[(tokens at: tokenCount  - 1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
					stamp _ tokens at: tokenCount]].
	file close.
	^ stamp! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/15/2018 16:45:44'!
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
	Smalltalk at: #Display put: DisplayScreen new.
	Smalltalk at: #SourceFiles put: (Array new: 2).
	Smalltalk openSourceFiles.

	"Here, startup begins!! (isARealStartup might be nil)"
	Smalltalk allClassesDo: [ :cls | cls initClassCachedState ].
	self doStartUp: isARealStartup == true.

	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject _ guiRootObjectClass newWorld ]).
	Display triggerEvent: #screenSizeChanged.
	
	self restoreLostChangesIfNecessary.

	UISupervisor whenUIinSafeState: [
		"guiRootObject fullRepaintNeeded."
		reopenTranscript ifTrue: [
			TranscriptWindow openTranscript ]].

	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [
		UISupervisor whenUIinSafeState: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].

	"Now it's time to raise an error"
	isARealStartup ifNil: [ self error: 'Failed to write image file (disk full?)' ].

] forkAt: Processor timingPriority-1 named: 'Startup process'.! !

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

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'jmv 1/15/2018 16:17:47'!
hasToRestoreChangesFrom: changesFile

	| chunk  |
	
	changesFile position: self lastQuitLogPosition.
	[ changesFile atEnd ] whileFalse: [ chunk := changesFile nextChunk ].
	
	^chunk notNil and: [(self isSnapshotQuitOrQuitNoSaveRecord: chunk) not]
! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 1/15/2018 15:51:11'!
actualScreenDepth
	<primitive: 'primitiveScreenDepth'>
	^ Display ifNotNil: [ Display depth ]! !


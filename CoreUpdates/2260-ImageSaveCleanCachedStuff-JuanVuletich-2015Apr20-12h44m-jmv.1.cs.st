'From Cuis 4.2 of 25 July 2013 [latest update: #2258] on 20 April 2015 at 12:47:24.756862 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/20/2015 11:49'!
logSapshot: save andQuit: quit
	"Log quitting to changes file"
	| msg |
	(SourceFiles at: 2) ifNotNil: [
		msg _ String streamContents: [ :s |
			s
				nextPutAll: '----';
				nextPutAll:
				(save
					ifTrue: [
						quit
							ifTrue: [ 'QUIT' ]
							ifFalse: [ 'SNAPSHOT' ]]
					ifFalse: [
						quit
							ifTrue: [ 'QUIT/NOSAVE' ]
							ifFalse: [ 'NOP' ]]);
				nextPutAll: '----';
				print: Date dateAndTimeNow;
				space;
				nextPutAll: (FileDirectory default localNameFor: self imageName);
				nextPutAll: ' priorSource: ';
				print: LastQuitLogPosition ].
		self assureStartupStampLogged.
		save ifTrue: [
			LastQuitLogPosition _ (SourceFiles at: 2)
				 setToEnd;
				 position ].
		self logChange: msg.
		Transcript
			 newLine;
			 show: msg;
			 newLine ]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/20/2015 12:02'!
printStuffToCleanOnImageSave
	"So far, to serve experiments.
	Some day, when this is empty, we'll be closer to bootstrap from sources."
	| n nonNilVars m classInstVars v|
	n _ 0.
	""
	'--------' print.
	'Proceeses: ' print.
	((Process allSubInstances reject: [:each | each isTerminated ])
		sort: [ :a :b | a priority >= b priority ]) do: [ :p | p print ].
	'======' print.
	'Globals: ' print.
	Smalltalk globals associationsDo: [ :g | " {g key. g value class }" g  print ].
	'======' print.
	'ClassVars (notNil):  ' print.
	n _ 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		nonNilVars _ cls classPool select: [ :var | var notNil ].
		nonNilVars notEmpty ifTrue: [
			n _ n + 1.
			{ cls. nonNilVars keys} print ]].
	'======' print.
	'InstClassVars (notNil): ' print.
	m _ 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		classInstVars _ Dictionary new.
		cls class allRegularInstVarNames do: [ :nam |
			v _ cls instVarNamed: nam.
			v ifNotNil: [
				classInstVars at: nam put: v ]].
		classInstVars notEmpty ifTrue: [
			m _ m + 1.
			{ cls class. classInstVars keys }  print ]].
	'======' print.
	
	{ n. m}  print! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/20/2015 12:47'!
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
	"Smalltalk printStuffToCleanOnImageSave."


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


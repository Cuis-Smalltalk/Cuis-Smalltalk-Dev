'From Cuis 4.2 of 25 July 2013 [latest update: #2299] on 3 May 2015 at 8:03:58.095583 pm'!

!Behavior methodsFor: 'system startup' stamp: 'jmv 5/3/2015 19:54'!
initClassCachedState
	"Will be called for each class on startup.
	See implementors for examples

	Can be used for eagerly initializing stuff that was cleared with releaseClassCachedState"! !


!ParseNode class methodsFor: 'class initialization' stamp: 'jmv 5/3/2015 19:39'!
initClassCachedState
	"
	ParseNode initialize. VariableNode initialize
	"
	LdInstType := 1.
	LdTempType := 2.
	LdLitType := 3.
	LdLitIndType := 4.
	SendType := 5.
	CodeBases := #(0 16 32 64 208 ).
	CodeLimits := #(16 16 32 32 16 ).
	LdSelf := 112.
	LdTrue := 113.
	LdFalse := 114.
	LdNil := 115.
	LdMinus1 := 116.
	LoadLong := 128.
	Store := 129.
	StorePop := 130.
	ShortStoP := 96.
	SendLong := 131.
	DblExtDoAll := 132.
	SendLong2 := 134.
	LdSuper := 133.
	Pop := 135.
	Dup := 136.
	LdThisContext := 137.
	EndMethod := 124.
	EndRemote := 125.
	Jmp := 144.
	Bfp := 152.
	JmpLimit := 8.
	JmpLong := 164.  "code for jmp 0"
	BtpLong := 168.
	SendPlus := 176.
	Send := 208.
	SendLimit := 16! !

!ParseNode class methodsFor: 'cached class state' stamp: 'jmv 5/3/2015 20:01'!
releaseClassCachedState

	LdInstType _ LdTempType _ LdLitType _ LdLitIndType _ SendType _ nil.
	CodeBases _ CodeLimits _ nil.
	LdSelf _ LdTrue _ LdFalse _ nil.
	LdNil _ LdMinus1 _ LoadLong _ Store _ StorePop _ ShortStoP _ nil.
	SendLong _ DblExtDoAll _ SendLong2 _ LdSuper _ Pop _ Dup _ nil.
	LdThisContext _ EndMethod _ EndRemote _ Jmp _ Bfp _ nil.
	JmpLimit _ JmpLong _ BtpLong _ SendPlus _ Send _ SendLimit _ nil! !


!VariableNode class methodsFor: 'class initialization' stamp: 'jmv 5/3/2015 19:43'!
initClassCachedState
	"
	VariableNode initialize
	"
	| encoder |
	encoder := Encoder new.
	StdVariables := Dictionary new: 16.
	encoder
		fillDict: StdVariables
		with: VariableNode
		mapping: #('self' 'thisContext' 'super' 'nil' 'false' 'true' )
		to: (Array with: LdSelf with: LdThisContext with: LdSuper)
				, (Array with: LdNil with: LdFalse with: LdTrue).
	StdSelectors := Dictionary new: 64.
	encoder
		fillDict: StdSelectors
		with: SelectorNode
		mapping: ((1 to: Smalltalk specialSelectorSize) collect: 
							[:i | Smalltalk specialSelectorAt: i])
		to: (SendPlus to: SendPlus + 31).
	StdLiterals := LiteralDictionary new: 16.
	encoder
		fillDict: StdLiterals
		with: LiteralNode
		mapping: #(-1 0 1 2 )
		to: (LdMinus1 to: LdMinus1 + 3).
	encoder initScopeAndLiteralTables.

	NodeNil := encoder encodeVariable: 'nil'.
	NodeTrue := encoder encodeVariable: 'true'.
	NodeFalse := encoder encodeVariable: 'false'.
	NodeSelf := encoder encodeVariable: 'self'.
	NodeThisContext := encoder encodeVariable: 'thisContext'.
	NodeSuper := encoder encodeVariable: 'super'! !

!VariableNode class methodsFor: 'class initialization' stamp: 'jmv 5/3/2015 20:02'!
releaseClassCachedState

	StdVariables _ nil.
	StdSelectors _ nil.
	StdLiterals _ nil.

	NodeNil _ nil.
	NodeTrue _ nil.
	NodeFalse _ nil.
	NodeSelf _ nil.
	NodeThisContext _ nil.
	NodeSuper _ nil ! !


!MessageNode class methodsFor: 'class initialization' stamp: 'jmv 5/3/2015 19:41'!
initClassCachedState
	"
	MessageNode initialize
	"
	MacroSelectors := 
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:).
	MacroTransformers := 
		#(	transformIfTrue: transformIfFalse: transformIfTrueIfFalse: transformIfFalseIfTrue:
			transformAnd: transformOr:
			transformWhile: transformWhile: transformWhile: transformWhile:
			transformToDo: transformToDo:
			transformCase: transformCase:
			transformIfNil: transformIfNil:  transformIfNilIfNotNil: transformIfNotNilIfNil:).
	MacroEmitters := 
		#(	emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:
			emitCodeForWhile:encoder:value: emitCodeForWhile:encoder:value:
			emitCodeForWhile:encoder:value: emitCodeForWhile:encoder:value:
			emitCodeForToDo:encoder:value: emitCodeForToDo:encoder:value:
			emitCodeForCase:encoder:value: emitCodeForCase:encoder:value:
			emitCodeForIfNil:encoder:value: emitCodeForIfNil:encoder:value:
			emitCodeForIf:encoder:value: emitCodeForIf:encoder:value:).
	MacroSizers := 
		#(	sizeCodeForIf:value: sizeCodeForIf:value: sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForIf:value: sizeCodeForIf:value:
			sizeCodeForWhile:value: sizeCodeForWhile:value: sizeCodeForWhile:value: sizeCodeForWhile:value:
			sizeCodeForToDo:value: sizeCodeForToDo:value:
			sizeCodeForCase:value: sizeCodeForCase:value:
			sizeCodeForIfNil:value: sizeCodeForIfNil:value: sizeCodeForIf:value: sizeCodeForIf:value:).
	MacroPrinters := 
		#(	printIfOn:indent: printIfOn:indent: printIfOn:indent: printIfOn:indent:
			printIfOn:indent: printIfOn:indent:
			printWhileOn:indent: printWhileOn:indent: printWhileOn:indent: printWhileOn:indent:
			printToDoOn:indent: printToDoOn:indent:
			printCaseOn:indent: printCaseOn:indent:
			printIfNil:indent: printIfNil:indent: printIfNilNotNil:indent: printIfNilNotNil:indent:)! !

!MessageNode class methodsFor: 'class initialization' stamp: 'jmv 5/3/2015 20:03'!
releaseClassCachedState

	MacroSelectors _ nil.
	MacroTransformers _ nil.
	MacroEmitters _ nil.
	MacroSizers _ nil.
	MacroPrinters _ nil! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/3/2015 19:57'!
snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: clearAllStateFlag
"WARNING: Current process will be killed.
UI Process will be restarted
"
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."
	"
	To test the cleanup and startup procedures, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: true
	"
| activeProc |
activeProc _ Processor activeProcess.
[ 
	| resuming world someTranscript |
	self logSapshot: save andQuit: quit.

	someTranscript _ false.
	TranscriptWindow allInstancesDo: [ :each | each isInWorld ifTrue: [ each delete. someTranscript _ true ]].
	Transcript logToFile: false.
	ActiveModel flushEventSystem.
	self processShutDownList: quit.


	"Lo que sigue aca podria ir al shutdown de alguien... (se levantan en startup!!)"
	Smalltalk stopLowSpaceWatcher.
	WeakArray stopFinalizationProcess.
	ProcessorScheduler stopBackgroundProcess.
	
	"Cosas que levanto explicitamente abajo"
	world _ ProjectX ui.
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
	false not ifTrue: [ Smalltalk printStuffToCleanOnImageSave ].


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
	Smalltalk at: #SourceFiles put: (Array new: 2).	Smalltalk openSourceFiles.

	"Here, startup begins!! (resuming might be nil)"
	Smalltalk allClassesDo: [ :cls | cls initClassCachedState ].
	self startup: resuming == true.
	
	
	ProjectX spawnNewMorphicProcessFor: world.
	
	someTranscript ifTrue: [
		WorldState addDeferredUIMessage: [
			TranscriptWindow openTranscript ]].
	WorldState addDeferredUIMessage: [
		world fullRepaintNeeded ].

	"Now it's time to raise an error"
	resuming ifNil: [ self error: 'Failed to write image file (disk full?)' ].

] forkAt: Processor timingPriority-1 named: 'Startup process'.! !


!ParseNode class methodsFor: 'class initialization' stamp: 'jmv 5/3/2015 19:42'!
initialize
	"
	ParseNode initialize. VariableNode initialize
	"
	self initClassCachedState! !


!VariableNode class methodsFor: 'class initialization' stamp: 'jmv 5/3/2015 19:42'!
initialize
	"
	VariableNode initialize
	"
	self initClassCachedState! !


!MessageNode class methodsFor: 'class initialization' stamp: 'jmv 5/3/2015 19:42'!
initialize
	"
	MessageNode initialize
	"
	self initClassCachedState! !

MessageNode initialize!
VariableNode initialize!
ParseNode initialize!

!ParseNode class reorganize!
('accessing' blockReturnCode popCode pushNilCode tempSortBlock)
('class initialization' initClassCachedState initialize)
('cached class state' releaseClassCachedState)
!


!Decompiler class reorganize!
('testing' recompileAllTest)
!


'From Cuis7.9 [latest update: #8161] on 9 September 2026 at 10:54:17 am'!
!classDefinition: #SystemDictionary category: #'Kernel-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments '
	classVariableNames: 'ChangesInitialFileSize EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore PreviousUserChangesName ShutDownList SourceFileVersionString StartUpList StartupStamp SystemAttributeCache DoingStartup '
	poolDictionaries: ''
	category: 'Kernel-Support'!

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 9/Sep/2026, 1:46:42 pm (UTC)'!
doingStartup
	^DoingStartup! !


!BacktickNode methodsFor: 'literal node' stamp: 'jmv 9/Sep/2026, 1:36:36 pm (UTC)'!
evaluateBacktickSourceCode
	Smalltalk doingStartup
		ifTrue: [ ^Compiler evaluate: sourceCode ].
		
	^[[[Compiler evaluate: sourceCode ]
		on: SyntaxErrorNotification
		do: [ :ex | parser notify: 'Can not compile: ', ex errorMessage at: range first ]]
		on: UndeclaredVariableReference
		do: [ :ex | parser notify: 'Can not compile: Variable ''', ex varName, ''' is not declared' at: range first ]]
		on: Error
		do: [ :ex | parser notify: 'Can not evaluate code: ', ex description at: range first ].! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 9/Sep/2026, 1:53:31 pm (UTC)'!
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
	"These could be moved to some #shutDown"
	self stopLowSpaceWatcher.
	ProcessorScheduler stopBackgroundProcess.
	"Be sure all unreferenced objects are finalized as approriate.
	Do this again later, as close to the snapshot / quit primitive as possible."
	self garbageCollect.
	EphemeronFinalizationProcess ensureFinalizationComplete.
	EphemeronFinalizationProcess stopFinalizationProcess.
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
			cls releaseClassState ].
		"Usually we set it during startup, in #readAndApplyUserPrefs"
		Utilities clearAuthor ].
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [
		Transcript clear.
		"Enable this call to actually see the image clean report."
		false ifTrue: [self printStuffToCleanOnImageSave]].
	(Cursor cursorAt: #writeCursor) activateCursor.
	"The snapshot primitive will run a GC, but any detected Ephemeron will not be finalized, and the finalization
	queue will be cleared by the VM both in running and snapshotted image. This means they will never be finalized.
	To avoid that, just before snapshot, do a GC.
	We also need to do #mournAllPending because TheFinalizationProcess has been already stopped. "
	self garbageCollect.
	EphemeronFinalizationProcess mournAllPending.
	"Do image save & quit as apropriate"
	isARealStartup := false.
	save ifTrue: [
		"The snapshot primitive answers false if it was just called to do the snapshot.
		But image startup is resumed by returning (again) from the primitive, but this time answering true.
		nil means saving image failed"
		isARealStartup := embeddedFlag
			ifTrue: [ self snapshotEmbeddedPrimitive ]
			ifFalse: [ self snapshotPrimitive ]].
	quit & (isARealStartup == false) ifTrue: [ self quitPrimitive ]. "Don't quit on nil (image save failed)"

	isARealStartup == true
		ifTrue: [
			DoingStartup := true ]
		ifFalse: [
			"Keep current #userBaseDirectory unless a real startup"
			DirectoryEntry setUserBaseDirectory: userBaseDirectory ].

	"Here, startup begins!! (isARealStartup might be nil)"
	self logStartupDebugAid: 'To #initClassCachedState'.
	self allClassesDo: [ :cls | cls initClassCachedState ].
	self logStartupDebugAid: 'To #doStartUp:'.
	self doStartUp: isARealStartup == true.
	"Some command line arguments need to be processed before starting the UI"
	self readCommandLineArguments.
	isARealStartup == true ifTrue: [
		self processCommandLineArguments: true ].
	"Recreate Global names"
	self at: #Sensor put: nil.
	self at: #SourceFiles put: (Array new: 2).
	isARealStartup == true
		ifTrue: [
			self logStartupDebugAid: 'To #readAndApplyUserPrefs'.
			self readAndApplyUserPrefs ].
	self logStartupDebugAid: 'To #openSourceFiles'.
	self openSourceFiles.
	self processStartUpList: isARealStartup.
	isARealStartup == true ifTrue: [
		self setStartupStamp ].
	self logStartupDebugAid: 'To #spawnNewMorphicProcessFor: ', guiRootObject printString.
	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject := guiRootObjectClass newWorld ]).
	isARealStartup == true
		ifTrue: [
			self logStartupDebugAid: 'To #checkIfAlreadyRunningOrStoppedNoExit'.
			self checkIfAlreadyRunningOrStoppedNoExit.
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
				self logStartupDebugAid: 'To #processCommandLineArguments'.
				"Some command line arguments need to be processed after the UI is up, and in the UI process"
				self processCommandLineArguments: false.	
				DoingStartup := false.
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
	named: 'Startup process'.
! !

!classDefinition: #SystemDictionary category: #'Kernel-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments'
	classVariableNames: 'ChangesInitialFileSize DoingStartup EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore PreviousUserChangesName ShutDownList SourceFileVersionString StartUpList StartupStamp SystemAttributeCache'
	poolDictionaries: ''
	category: 'Kernel-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
SystemDictionary classPool at: #DoingStartup put: true.
UISupervisor whenUIinSafeState: [ SystemDictionary classPool at: #DoingStartup put: false ].!


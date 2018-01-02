'From Cuis 5.0 of 7 November 2016 [latest update: #3239] on 28 December 2017 at 3:15:30 pm'!

!UISupervisor commentStamp: '<historical>' prior: 0!
UISupervisor is an interface to User Interface services not tied to any specific GUI. There could even be no GUI.
All state and behavior is on the class side!


!Debugger class methodsFor: 'opening' stamp: 'jmv 12/28/2017 15:02:53'!
interruptProcess: aProcess label: labelString
	"Create a Notifier on aProcess with the given label."
	|  label |

	label _ labelString,
					' - Process: ', aProcess name,
					' - Priority: ', aProcess priority printString.
	aProcess isTerminated
		ifTrue: [
			UISupervisor newProcessIfUI: aProcess.
			self notify: 'Can not debug a terminated process: ', label ]
		ifFalse: [
			aProcess suspend.
			self
				openInterrupt: label 
				onProcess: aProcess ]! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 12/28/2017 14:50:40'!
newProcessIfUI: suspendedProcess
	"Answer the UI we created a new process for"

	suspendedProcess animatedUI ifNotNil: [ :guiRootObject |
		self spawnNewMorphicProcessFor: guiRootObject.
		^guiRootObject ].
	^nil! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 12/28/2017 15:04:16'!
userInterrupt
	"Create a Notifier on the active scheduling process with the given label."
	| preemptedProcess  |
	preemptedProcess _ Processor preemptedProcess.

	"Only debug preempted process if its priority is >= UIProcess' priority"
	preemptedProcess priority < UIProcess priority ifTrue: [
		preemptedProcess _ UIProcess ].

	Debugger interruptProcess: preemptedProcess label: 'User Interrupt'! !


!PasteUpMorph methodsFor: 'interaction loop' stamp: 'jmv 12/28/2017 14:38:23'!
mainLoop

	
	self clearWaitDelay.
	self clearCanvas.
	[
		self doOneCycle.
		Processor yield.
		true ]
			whileTrue: []! !

!PasteUpMorph methodsFor: 'interaction loop' stamp: 'jmv 12/28/2017 14:40:22'!
runProcess
	| process |
	process _ [ self mainLoop ]
		newProcess.
	process
		priority: Processor userSchedulingPriority;
		name: 'Morphic UI';
		animatedUI: self.
	^ process! !


!Debugger methodsFor: 'initialization' stamp: 'jmv 12/28/2017 14:43:57'!
openFullNoSuspendLabel: aString
	"Create and schedule a full debugger with the given label. Do not terminate the current active process."

	self openFullMorphicLabel: aString.
	interruptedProcessUI _ UISupervisor newProcessIfUI: interruptedProcess! !

!Debugger methodsFor: 'initialization' stamp: 'jmv 12/28/2017 14:44:04'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor normal activateCursor.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self lowSpaceChoices, (msgString ifNil: ['']) ]
		ifFalse: [ msgString ].

	interruptedProcessUI _ UISupervisor newProcessIfUI: interruptedProcess.
	WorldState addDeferredUIMessage: [
		PreDebugWindow open: self label: label message: msg ].
	^self! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 12/28/2017 14:44:13'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."
	| w |
	Preferences logDebuggerStackToFile ifTrue: [
		Smalltalk
			logError: title
			inContext: context
			to: 'CuisDebug' ].
	w := UISupervisor newProcessIfUI: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	WorldState addDeferredUIMessage: [
		[ | debugger |
		"In case an error in Morphic code got us here, ensure mouse focus has been released"
		true runningWorld ifNotNil: [ :rWorld |
			rWorld activeHand ifNotNil: [ :aHand |
				aHand releaseMouseFocus ]].
		debugger := self new
			process: process
			context: context.
		debugger interruptedProcessUI: w.
		bool
			ifTrue: [ debugger openFullMorphicLabel: title ]
			ifFalse: [
				PreDebugWindow
					open: debugger
					label: title
					message: nil ]]
			on: UnhandledError
			do: [ :exOuter | | errorDescription |
				errorDescription := 'Error while trying to open Debugger' , String newLineString , 'Orginal error: ' , title asString , '.' , String newLineString , '	Debugger error: ' ,
					([ exOuter description ]
						on: UnhandledError
						do: [ :exInner |
							exInner return: 'a ' , exInner class printString ]) , ':'.
				self primitiveError: errorDescription ]].
	process suspend.! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 12/28/2017 15:00:19'!
lowSpaceWatcher
	"Wait until the low space semaphore is signalled, then take appropriate actions."

	self garbageCollectMost <= self lowSpaceThreshold ifTrue: [
		self garbageCollect <= self lowSpaceThreshold ifTrue: [
			"free space must be above threshold before starting low space watcher"
			^ Smalltalk primitiveBeep ]].

	LowSpaceSemaphore _ Semaphore new.
	self primLowSpaceSemaphore: LowSpaceSemaphore.
	self primSignalAtBytesLeft: self lowSpaceThreshold.  "enable low space interrupts"

	LowSpaceSemaphore wait.  "wait for a low space condition..."

	self primSignalAtBytesLeft: 0.  "disable low space interrupts"
	self primLowSpaceSemaphore: nil.
	LowSpaceProcess _ nil.
	"Note: user now unprotected until the low space watcher is re-installed"

	Debugger interruptProcess: Processor preemptedProcess label: 'Space is low'! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 12/28/2017 15:04:08'!
handleUserInterrupt
	Preferences cmdDotEnabled ifTrue: [
		[ UISupervisor userInterrupt ] fork]! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/28/2017 14:48:02'!
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
	| isARealStartup guiRootObject reopenTranscript |
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

	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject _ self privateMorphicWorldClass newWorld ]).

	reopenTranscript ifTrue: [
		guiRootObject addDeferredUIMessage: [
			TranscriptWindow openTranscript ]].
	"
	world  addDeferredUIMessage: [
		world fullRepaintNeeded ].
	"

	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [	
		guiRootObject  addDeferredUIMessage: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].

	"Now it's time to raise an error"
	isARealStartup ifNil: [ self error: 'Failed to write image file (disk full?)' ].

] forkAt: Processor timingPriority-1 named: 'Startup process'.! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 12/28/2017 14:50:33'!
spawnNewMorphicProcessFor: guiRootObject

	UIProcess ifNotNil: [ UIProcess animatedUI: nil ].
	UIProcess _ guiRootObject runProcess.
	UIProcess resume! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 12/28/2017 15:10:48'!
stopUIProcess
	UIProcess ifNotNil: [
		UIProcess animatedUI: nil.
		UIProcess terminate ].
	UIProcess _ nil! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 12/28/2017 15:09:33'!
ui
	^UIProcess animatedUI! !

!methodRemoval: UISupervisor class #currentInterruptNameX:!
UISupervisor class removeSelector: #currentInterruptNameX:!
!methodRemoval: UISupervisor class #interruptNameX:!
UISupervisor class removeSelector: #interruptNameX:!
!methodRemoval: UISupervisor class #newProcessIfUIX:!
UISupervisor class removeSelector: #newProcessIfUIX:!
!methodRemoval: SystemDictionary #privateMorphicTopLevelRendererClass!
SystemDictionary removeSelector: #privateMorphicTopLevelRendererClass!

!UISupervisor class reorganize!
('gui process' newProcessIfUI: spawnNewMorphicProcessFor: stopUIProcess ui userInterrupt)
!


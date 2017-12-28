'From Cuis 5.0 of 7 November 2016 [latest update: #3239] on 28 December 2017 at 3:45:45 pm'!

!BlockClosure commentStamp: 'jmv 12/28/2017 15:42:10' prior: 0!
I am a block closure for Eliot's closure implementation. Not to be confused with the old BlockClosure (they were never part of Cuis anyway).

This is a closure converted image. With full closure support, you can finally use recursive blocks like here:

| fac |
	fac := [:n| n > 1 ifTrue:[n * (fac value: n-1)] ifFalse:[1]].
	fac value: 5. "120"

and close over temps correctly, such as here:

	(1 to: 10) do:[:i| UISupervisor whenUIinSafeState:[Transcript newLine; show: i]].

Another good example:

| fib |
fib := [:n| n < 2 ifTrue:[1] ifFalse:[(fib value:n-1) + (fib value:n-2)]].
fib value: 10. "89"!


!UISupervisor class methodsFor: 'services' stamp: 'jmv 12/28/2017 15:36:24'!
restoreDisplay
	DisplayScreen startUp.
	self ui ifNotNil: [ :guiRootObject |
		guiRootObject restoreDisplay ].
	UISupervisor whenUIinSafeState: [ Cursor normal activateCursor ]! !

!UISupervisor class methodsFor: 'services' stamp: 'jmv 12/28/2017 15:29:34'!
whenUIinSafeState: evaluableObject
	"If there is an UI present, evaluate argument when such UI is in a safe state.
	If not, just evaluate the argument right now."
	self ui
		ifNotNil: [ :guiRootObject | guiRootObject whenUIinSafeState: evaluableObject ]
		ifNil: evaluableObject! !


!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 12/28/2017 15:36:22'!
restoreDisplay
	self
		morphExtent: Display extent;
		handsDo: [ :h | h visible: true ];
		fullRepaintNeeded! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 12/28/2017 15:30:26'!
whenUIinSafeState: evaluableObject
	"Please call
	UISupervisor whenUIinSafeState: evaluableObject
	"
	WorldState addDeferredUIMessage: evaluableObject ! !


!Debugger methodsFor: 'initialization' stamp: 'jmv 12/28/2017 15:31:17'!
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
	UISupervisor whenUIinSafeState: [
		PreDebugWindow open: self label: label message: msg ].
	^self! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 12/28/2017 15:31:22'!
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
	UISupervisor whenUIinSafeState: [
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


!TestRunner methodsFor: 'processing' stamp: 'jmv 12/28/2017 15:41:46'!
runSuite: suite
	running ifNotNil: [ ^self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	running _ [
            [ result _ suite run ]
	            ensure: [
		            running _ nil.
				suite removeDependent: self.
				runSemaphore signal.
				UISupervisor whenUIinSafeState: [
					self updateWindow: result.
			      	self changed: #runTests.
			      	self changed: #runOneTest.
				].
	            ].
     ] newProcess.
	self runWindow.
      self changed: #runTests.
      self changed: #runOneTest.
      running
		name: 'TestRunner';
"	      priority: Processor userBackgroundPriority;"
	      resume.
! !

!TestRunner methodsFor: 'processing' stamp: 'jmv 12/28/2017 15:41:57'!
runSuiteProfiled: suite
	running ifNotNil: [ ^self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	running _ [
            [ result _ MessageTally spyOn: [suite run] ]
	            ensure: [
		            running _ nil.
				suite removeDependent: self.
				runSemaphore signal.
				UISupervisor whenUIinSafeState: [
					self updateWindow: result.
			      	self changed: #runTests.
			      	self changed: #runOneTest.
				].
	            ].
     ] newProcess.
	self runWindow.
      self changed: #runTests.
      self changed: #runOneTest.
      running
		name: 'TestRunner';
"	      priority: Processor userBackgroundPriority;"
	      resume.
! !

!TestRunner methodsFor: 'updating' stamp: 'jmv 12/28/2017 15:41:51'!
update: aParameter 
	"updates come in from another thread"
	(aParameter is: #TestCase)
		ifTrue: [
			UISupervisor whenUIinSafeState: [
				completedTests _ completedTests + 1.
				progressLabel _ aParameter printString.
				self changed: #progress ]]
		ifFalse: [ super update: aParameter ]! !


!CPUWatcher methodsFor: 'porcine capture' stamp: 'jmv 12/28/2017 15:31:10'!
openWindowForSuspendedProcess: aProcess 

	UISupervisor whenUIinSafeState: [self openMorphicWindowForSuspendedProcess: aProcess]! !


!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 12/28/2017 15:42:34'!
dumpPigStackOn: aStream
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process depth stack suspendedContext |
	promise := Processor tallyCPUUsageFor: 1 every: 10.
	tally := promise value.
	"UISupervisor whenUIinSafeState: [self dumpTallyOnTranscript: tally]."
	aStream nextPutAll: '====Al processes===='; newLine.
	self dumpTally: tally on: aStream.
	aStream newLine; nextPutAll: '====Process using most CPU===='; newLine.
	process _ tally sortedCounts first value.
	(100.0 * (tally occurrencesOf: process) / tally size) rounded printOn: aStream.
	aStream
		nextPutAll: ' % ';
		nextPutAll: (process browserPrintStringFull: false);
		newLine.
	depth _ 20.
	stack _ process == Processor activeProcess
		ifTrue: [thisContext stackOfSize: depth]
		ifFalse: [suspendedContext _ process suspendedContext.
			suspendedContext
				ifNotNil: [suspendedContext stackOfSize: depth]].
	stack 
		ifNil: [ aStream nextPutAll: 'No context'; newLine]
		ifNotNil: [
			stack do: [ :c | 
				c printOn: aStream.
				aStream newLine ]]! !


!MessageTally class methodsFor: 'spying' stamp: 'jmv 12/28/2017 15:31:52'!
tallySendsTo: receiver inBlock: aBlock
	"
	MessageTally tallySends: [3.14159 printString]
	"
	"This method uses the simulator to count the number of calls on each method
	invoked in evaluating aBlock. If receiver is not nil, then only sends
	to that receiver are tallied.
	Results are presented as leaves, sorted by frequency,
	preceded, optionally, by the whole tree."
	| prev tallies startTime totalTime |
	startTime _ Time localMillisecondClock.
	tallies _ self new class: aBlock receiver class method: aBlock method.
	prev _ aBlock.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [ :current |
			current == prev ifFalse: [ "call or return"
				prev sender ifNotNil: [ "call only"
					(receiver == nil or: [ current receiver == receiver ])
						ifTrue: [ tallies tally: current by: 1 ]].
				prev _ current]].

	totalTime _ Time localMillisecondClock - startTime / 1000.0.
	UISupervisor whenUIinSafeState: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [ :s |
				s
					nextPutAll: 'This simulation took ';
					nextPutAll: totalTime printString;
					nextPutAll: ' seconds.';
					newLine.
				tallies fullPrintExactOn: s ]))
			label: 'Spy Results'
			wrap: false ]! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 12/28/2017 15:42:48'!
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

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'jmv 12/28/2017 15:40:12'!
restoreLostChangesIfNecessary

	"
	Smalltalk restoreLostChangesIfNecessary
	"
	
	self hasToRestoreChanges ifTrue: [ UISupervisor whenUIinSafeState: [self restoreLostChanges ]].
! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 12/28/2017 15:26:47'!
ui
	^UIProcess ifNotNil: [ UIProcess animatedUI ]! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 12/28/2017 15:27:03'!
userInterrupt
	"Create a Notifier on the active scheduling process with the given label."
	| preemptedProcess  |
	preemptedProcess _ Processor preemptedProcess.

	"Only debug preempted process if its priority is >= UIProcess' priority"
	UIProcess ifNotNil: [
		preemptedProcess priority < UIProcess priority ifTrue: [
			preemptedProcess _ UIProcess ]].

	Debugger interruptProcess: preemptedProcess label: 'User Interrupt'! !


!DisplayScreen methodsFor: 'other' stamp: 'jmv 12/28/2017 15:22:40'!
newDepth: pixelSize
"
	Display newDepth: 8.
	Display newDepth: 1.
"
	(self supportsDisplayDepth: pixelSize)
		ifFalse: [ ^self inform:'Display depth ', pixelSize printString, ' is not supported on this system' ].
	self newDepthNoRestore: pixelSize.
	self runningWorld ifNotNil: [ :w | w  buildMagnifiedBackgroundImage ].
	self restore.! !


!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 12/28/2017 15:37:02'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	Display extent = DisplayScreen actualScreenSize ifTrue: [^ self].
	UISupervisor restoreDisplay! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 12/28/2017 15:33:32'!
showBalloon: msgString hand: aHand
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| w balloon h |
	(w _ self world) ifNil: [^ self].
	h _ aHand.
	h ifNil:[
		h _ w activeHand].
	balloon _ HoverHelpMorph contents: msgString.
	
	"Do it in a while. In some cases, processing the event that might have triggered us might also remove any Help Balloon"
	UISupervisor whenUIinSafeState: [
		balloon popUpForHand: h ]! !

!Morph methodsFor: 'initialization' stamp: 'jmv 12/28/2017 15:32:34'!
openInWorld

	self runningWorld
		ifNil: [ UISupervisor whenUIinSafeState: [ self openInWorld ]]
		ifNotNil: [ :w | self openInWorld: w ]! !


!PasteUpMorph class methodsFor: 'system startup' stamp: 'jmv 12/28/2017 15:37:37'!
initClassCachedState

	"Should use some other way to find relevant instances"
	self flag: #jmvVer2.
	self allInstancesDo: [ :each |
		each buildMagnifiedBackgroundImage.
		each isWorldMorph ifTrue: [
			each restoreDisplay ]].
	Cursor normal activateCursor! !


!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 12/28/2017 15:31:28'!
arrowKey: aChar
	"Handle a keyboard navigation character. Answer true if handled, false if not."
	| keyEvent answer nextSelection oldSelection |
	answer := false.
	keyEvent := aChar numericValue.
	oldSelection := self getSelectionIndex.
	nextSelection := oldSelection.
	keyEvent = 31 ifTrue: [
		"down"
		nextSelection := oldSelection + 1.
		nextSelection > self maximumSelection ifTrue: [ nextSelection := self minimumSelection ]].
	keyEvent = 30 ifTrue: [
		"up"
		nextSelection := oldSelection - 1.
		nextSelection < self minimumSelection ifTrue: [ nextSelection := self maximumSelection ]].
	keyEvent = 1 ifTrue: [ "home"
		nextSelection := 1 ].
	keyEvent = 4 ifTrue: [ "end"
		nextSelection := scroller submorphs size ].
	keyEvent = 11 ifTrue: [ "page up"
		nextSelection := oldSelection - self numSelectionsInView max: 1 ].
	keyEvent = 12 ifTrue: [ "page down"
		nextSelection := oldSelection + self numSelectionsInView ].
	keyEvent = 29 ifTrue: [
		"right"
		selectedMorph ifNotNil: [
			(selectedMorph canExpand and: [ selectedMorph isExpanded not ])
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					self scrollSelectionIntoView.
					answer := true ]
				ifFalse: [ nextSelection := oldSelection + 1 ]]].
	keyEvent = 28 ifTrue: [
		"left"
		selectedMorph ifNotNil: [
			selectedMorph isExpanded
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					self scrollSelectionIntoView.
					answer := true ]
				ifFalse: [
					oldSelection > self minimumSelection ifTrue: [
						nextSelection _ (oldSelection-1 to: 1 by: -1) detect: [ :i | ( scroller submorphs at: i) indentLevel < selectedMorph indentLevel ]].
					]]].
	nextSelection = oldSelection ifFalse: [
		"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
		self highlightedMorph: (self listMorphAt: nextSelection).
		"Update the model in next world cycle, so user gets the immediate feedback."
		UISupervisor whenUIinSafeState: [ self setSelectionIndex: nextSelection ].
		answer := true ].
	^ answer.! !


!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 12/28/2017 15:39:41'!
arrowKey: aChar
	"Process the up and down arrows in a list pane.  Note that the listView tells us what index variable, how to get the list, and how to move the index."
	| keyEvent oldSelection nextSelection max min howMany answer w |
	answer _ false.
	keyEvent _ aChar numericValue.
	oldSelection _ self getCurrentSelectionIndex.
	nextSelection _ oldSelection.
	max _ self maximumSelection.
	min _ self minimumSelection.
	howMany _ self numSelectionsInView.	"get this exactly??"

	keyEvent = 31 ifTrue: [
		"down-arrow; move down one, wrapping to top if needed"
		nextSelection _ oldSelection + 1.
		nextSelection > max ifTrue: [ nextSelection _ 1 ]].

	keyEvent = 30 ifTrue: [
		"up arrow; move up one, wrapping to bottom if needed"
		nextSelection _ oldSelection - 1.
		nextSelection < 1 ifTrue: [ nextSelection _ max ]].

	keyEvent = 1 ifTrue: [ nextSelection _ 1 ].			"home"
	keyEvent = 4 ifTrue: [ nextSelection _ max ].		"end"
	keyEvent = 11 ifTrue: [ nextSelection _ min max: oldSelection - howMany ].		"page up"
	keyEvent = 12 ifTrue: [ nextSelection _ oldSelection + howMany min: max ].		"page down"
	nextSelection = oldSelection ifFalse: [
		w _ self owningWindow.
		(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

			"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
			self listMorph highlightedRow: nextSelection.

			"Update the model in next world cycle, so user gets the immediate feedback."
			UISupervisor whenUIinSafeState: [
				self changeModelSelection: nextSelection ].
			answer _ true ]].
	^answer! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 12/28/2017 15:39:45'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| oldIndex oldVal row w |
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].

	row _ self rowAtLocation: localEventPosition.

	row = 0 ifTrue: [
		^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
	self listMorph highlightedRow: row.

	w _ self owningWindow.
	(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

		"Set meaning for subsequent dragging of selection"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row.
		oldIndex _ self getCurrentSelectionIndex.
		oldIndex ~= 0 ifTrue: [oldVal _ self listSelectionAt: oldIndex].

		"Different from PluggableListMorph. There, we highlight on mouse down, and act on mouse up.
		Here, we act on mouse down, because we support dragging of selection, so mouse up will
		only happen after user is finished dragging. In order to get the highlight visible for the user,
		update the model on next world cycle."
		UISupervisor whenUIinSafeState: [
			"Set or clear new primary selection (listIndex)"
			dragOnOrOff
				ifTrue: [self changeModelSelection: row]
				ifFalse: [self changeModelSelection: 0].
			"Need to restore the old one, due to how model works, and set new one."
			oldIndex ~= 0 ifTrue: [self listSelectionAt: oldIndex put: oldVal].
			self listSelectionAt: row put: dragOnOrOff ].
	].
	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !


!SystemWindow methodsFor: 'open/close' stamp: 'jmv 12/28/2017 15:41:35'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| frameRect |
	frameRect _ self initialFrameIn: aWorld.
	self morphExtent: frameRect extent.
	aWorld addMorph: self position: frameRect topLeft.
	"Do it deferred. Was needed for text cursor to start blinking if (Preferences disable: #focusFollowsMouse) "
	UISupervisor whenUIinSafeState: [ self activate ]! !


!TaskbarMorph methodsFor: 'events' stamp: 'jmv 12/28/2017 15:41:40'!
screenSizeChanged
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	| y e |
	UISupervisor whenUIinSafeState: [
		self world ifNotNil: [ :w |
			y _ w morphExtent y - self defaultHeight.
			e _ self internalizeDistance: w morphExtent x @ self defaultHeight.
			self morphPosition: 0@y extent: e ]]! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 12/28/2017 15:31:44'!
acceptContents
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| accepted prevSelection prevScrollValue |
	
	prevSelection _ self editor selectionInterval copy.
	prevScrollValue _ owner verticalScrollBar scrollValue.
	
	(self canDiscardEdits and: [(self hasProperty: #alwaysAccept) not]) ifTrue: [self flash. ^true].
	(self hasEditingConflicts and: [ self confirmAcceptAnyway not ]) ifTrue: [self flash. ^false].
	
	accepted _ model acceptContentsFrom: owner.
	"During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the code-holding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
	
	"some implementors of acceptContentsFrom: answer self :("
	^accepted == true 
		ifTrue: [
			model refetch.
			self editor selectFrom: prevSelection first to: prevSelection last.
			UISupervisor whenUIinSafeState: [
				self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ].
				owner verticalScrollBar internalScrollValue: prevScrollValue].
			true]
		ifFalse: [ false ]! !


!WorldState class methodsFor: 'class initialization' stamp: 'jmv 12/28/2017 15:26:18'!
addDeferredUIMessage: evaluableObject
	"Objects should not be calling directly as this will disappear from this location!!  From the sender, instead to schedule on the currently running world use:
	self runningWorld addDeferredUIMessage: ...
	And to schedule on the world a given Morph exists in use:
	self world addDeferredUIMessage: ..."
	self deferredUIMessages nextPut: evaluableObject! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 12/28/2017 15:38:38'!
fullScreenOff

	Display fullScreenMode: false.
	DisplayScreen checkForNewScreenSize.
	myWorld restoreDisplay! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 12/28/2017 15:38:40'!
fullScreenOn

	Display fullScreenMode: true.
	DisplayScreen checkForNewScreenSize.
	myWorld restoreDisplay! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 12/28/2017 15:37:53'!
buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	| menu |
	menu _ MenuMorph new defaultTarget: self.
	self colorForDebugging: menu.
	menu addStayUpIcons.
	self fillIn: menu
		from: {
				{ 'Open...'. { self. #openWindow}}.
				{ 'New morph...'. { self. #newMorph}.
					'Offers a variety of ways to create new objects'}.
				{ 'Preferences...'. { self. #preferencesDo}.
					'put up a menu offering many controls over appearance and system preferences.'}.
				{ 'Windows...'. { self. #windowsDo}}.
				{ 'Help...'. { self. #helpDo}.
					'puts up a menu of useful items for updating the system, determining what version you are running, and much else'}.
				nil.
				{ 'Changes...'. { self. #changesDo}}.
				{ 'Debug...'. { self. #debugDo}.
					'a menu of debugging items'}.
				{ 'Restore Display (r)'. { myWorld. #restoreDisplay}.
					'repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'}.
				nil.
				{ 'Save'. { Smalltalk . #saveSession}.
					'save the current version of the image on disk'}.
				{ 'Save as...'. { Smalltalk . #saveAs}.
					'save the current version of the image on disk under a new name.'}.
				{ 'Save as New Version'. { Smalltalk . #saveAsNewVersion}.
					'give the current image a new version-stamped name\and save it under that name on disk.\Clear all user preferences and user state (class vars).' withNewLines}.
				{ 'Save and Quit'. { self. #saveAndQuit}.
					'save the image and quit out of Cuis.'}.				
				{ 'Quit'. { self. #quitSession}.
					'quit out of Cuis.'}}.
	^menu! !


!Theme class methodsFor: 'class initialization' stamp: 'jmv 12/28/2017 15:38:58'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 releaseClassCachedState.
	self runningWorld ifNotNil: [ :w |
		w backgroundImage ifNil: [
			w color: CurrentTheme background ]].
	SystemWindow initialize.
	BitBltCanvas releaseClassCachedState.
	self runningWorld ifNotNil: [ :w |
		CurrentTheme useTaskbar
			ifTrue: [w showTaskbar]
			ifFalse: [w hideTaskbar].
		w restoreDisplay ].
	
	^ CurrentTheme! !


!ProgessiveTestRunner methodsFor: 'evaluating - private' stamp: 'jmv 12/28/2017 15:40:01'!
runSuiteShowingProgress

	[ self registerTestSuiteAction. 
	progressBar openInWorld.
	self runSuite ] ensure: [
		self unregisterTestSuiteAction.
		UISupervisor whenUIinSafeState: [progressBar dismissMorph] ].
	! !


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 12/28/2017 15:31:01'!
doReport
	"Report the results of this profiler run"
	| report |
	report _ self report.
	UISupervisor whenUIinSafeState: [
		SystemWindow
			editText: (Workspace withText: report)
			label: 'Spy Results'
			wrap: false ]! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 12/28/2017 15:40:06'!
styleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, by triggering #shoutStyled
	The the model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.

	self mutex critical: [
		"This part runs at low priority, and signals sem when finished"
		backgroundProcess _  [
			self privateStyle.
			UISupervisor whenUIinSafeState: [
				textModel changed: #shoutStyled ].
			] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]! !


!PopUpMenu class methodsFor: 'dialogs' stamp: 'jmv 12/28/2017 15:39:54'!
inform: aString
	"PopUpMenu inform: 'I like Cuis'"

	UISupervisor whenUIinSafeState: [ (PopUpMenu labels: ' OK ') startUpWithCaption: aString ]! !

!methodRemoval: PasteUpMorph #addDeferredUIMessage:!
PasteUpMorph removeSelector: #addDeferredUIMessage:!
!methodRemoval: PasteUpMorph #restoreMorphicDisplay!
PasteUpMorph removeSelector: #restoreMorphicDisplay!

!UISupervisor class reorganize!
('gui process' newProcessIfUI: spawnNewMorphicProcessFor: stopUIProcess ui userInterrupt)
('services' restoreDisplay whenUIinSafeState:)
!


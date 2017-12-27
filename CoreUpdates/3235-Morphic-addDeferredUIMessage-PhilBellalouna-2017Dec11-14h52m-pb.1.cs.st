'From Cuis 5.0 of 7 November 2016 [latest update: #3227] on 11 December 2017 at 5:30:58 pm'!

!PasteUpMorph methodsFor: 'world state' stamp: 'pb 12/11/2017 15:25:45'!
addDeferredUIMessage: valuableObject
	"This will be safe to call directly in a multi-world environment (as opposed to the WorldState class-side method)"
	WorldState addDeferredUIMessage: valuableObject ! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'pb 12/11/2017 17:07:49'!
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
	world _ self privateMorphicTopLevelRendererClass ui.
	"Replace with this to create a new world at startup after 'saveAsNewVersion'"
	"world _ clearAllStateFlag ifFalse: [ self privateMorphicTopLevelRendererClass ui ]."
	self privateMorphicTopLevelRendererClass stopUIProcess.
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
	
	
	self privateMorphicTopLevelRendererClass spawnNewMorphicProcessFor: (world ifNil: [ world:=self privateMorphicWorldClass newWorld ]).

	reopenTranscript ifTrue: [
		world addDeferredUIMessage: [
			TranscriptWindow openTranscript ]].
	"
	world  addDeferredUIMessage: [
		world fullRepaintNeeded ].
	"

	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [	
		world  addDeferredUIMessage: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].

	"Now it's time to raise an error"
	isARealStartup ifNil: [ self error: 'Failed to write image file (disk full?)' ].

] forkAt: Processor timingPriority-1 named: 'Startup process'.! !


!Morph methodsFor: 'halos and balloon help' stamp: 'pb 12/11/2017 15:29:32'!
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
	self world addDeferredUIMessage: [
		balloon popUpForHand: h ]! !


!PasteUpMorph methodsFor: 'world state' stamp: 'pb 12/11/2017 15:30:12'!
restoreMorphicDisplay
	DisplayScreen startUp.
	self
		morphExtent: Display extent;
		handsDo: [ :h | h visible: true ];
		fullRepaintNeeded.
	self addDeferredUIMessage: [ Cursor normal activateCursor ]! !


!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'pb 12/11/2017 15:26:29'!
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
					answer := true ]
				ifFalse: [ nextSelection := oldSelection + 1 ]]].
	keyEvent = 28 ifTrue: [
		"left"
		selectedMorph ifNotNil: [
			selectedMorph isExpanded
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					answer := true ]
				ifFalse: [ nextSelection := oldSelection - 1 max: 1 ]].
		^ true ].
	nextSelection = oldSelection ifFalse: [
		"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
		self highlightedMorph: (self listMorphAt: nextSelection).
		"Update the model in next world cycle, so user gets the immediate feedback."
		self world addDeferredUIMessage: [ self setSelectionIndex: nextSelection ].
		answer := true ].
	^ answer.! !


!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'pb 12/11/2017 15:30:36'!
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
			self world addDeferredUIMessage: [
				self changeModelSelection: nextSelection ].
			answer _ true ]].
	^answer! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'pb 12/11/2017 15:30:48'!
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
		self world addDeferredUIMessage: [
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


!SystemWindow methodsFor: 'open/close' stamp: 'pb 12/11/2017 15:31:54'!
openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| frameRect |
	frameRect _ self initialFrameIn: aWorld.
	self morphExtent: frameRect extent.
	aWorld addMorph: self position: frameRect topLeft.
	"Do it deferred. Was needed for text cursor to start blinking if (Preferences disable: #focusFollowsMouse) "
	aWorld addDeferredUIMessage: [ self activate ]! !


!TaskbarMorph methodsFor: 'events' stamp: 'pb 12/11/2017 15:32:05'!
screenSizeChanged
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	| y e |
	self world addDeferredUIMessage: [
		self world ifNotNil: [ :w |
			y _ w morphExtent y - self defaultHeight.
			e _ self internalizeDistance: w morphExtent x @ self defaultHeight.
			self morphPosition: 0@y extent: e ]]! !


!InnerTextMorph methodsFor: 'editing' stamp: 'pb 12/11/2017 15:29:07'!
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
			"FIXME - is there actually a case where world will be nil here?"
			self world addDeferredUIMessage: [
				self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ].
				owner verticalScrollBar internalScrollValue: prevScrollValue].
			true]
		ifFalse: [ false ]! !


!WorldState class methodsFor: 'class initialization' stamp: 'pb 12/11/2017 15:33:57'!
addDeferredUIMessage: valuableObject
	"Objects should not be calling directly as this will disappear from this location!!  From the sender, instead to schedule on the currently running world use:
	self runningWorld addDeferredUIMessage: ...
	And to schedule on the world a given Morph exists in use:
	self world addDeferredUIMessage: ..."
	self deferredUIMessages nextPut: valuableObject! !


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'pb 12/11/2017 15:33:07'!
doReport
	"Report the results of this profiler run"
	self runningWorld addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: self report)
			label: 'Spy Results'
			wrap: false ]! !


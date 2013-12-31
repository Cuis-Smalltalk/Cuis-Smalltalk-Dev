'From Cuis 4.0 of 21 April 2012 [latest update: #1473] on 23 October 2012 at 11:08:52 pm'!

!Object methodsFor: 'morphic' stamp: 'jmv 10/23/2012 17:41'!
runningWorld
	"Answer a morphic world that is the current UI focus.
	This is the UI root animated by the active Process.
	This method could answer nil, if not in an UI process!!"

	^Processor activeProcess animatedUI! !


!Object methodsFor: 'private' stamp: 'jmv 10/23/2012 17:33'!
primitiveError: aString 
	"This method is called when the error handling results in a recursion in 
	calling on error: or halt or halt:."

	| context emergencyEvaluator lines r |
	r _ 10@10 extent: (Display extent -20 min: 700@1000).
	lines _ r height // StrikeFont default height.
	emergencyEvaluator _ Transcripter newInFrame: r.
	emergencyEvaluator
		nextPutAll: '***System error handling failed***'; newLine;
		nextPutAll: aString; newLine;
		nextPutAll: '-------------------------------'; newLine.
	context _ thisContext sender sender.
	(30 min: lines - 10) timesRepeat: [context ifNotNil: [emergencyEvaluator print: (context _ context sender); newLine]].
	emergencyEvaluator
		nextPutAll: '-------------------------------'; newLine;
		nextPutAll: 'Type ''revert'' to revert your last method change.'; newLine;
		nextPutAll: 'Type ''exit'' to exit the emergency evaluator.'; newLine.
	emergencyEvaluator readEvalPrint! !


!AbstractSound methodsFor: 'file i/o' stamp: 'jmv 10/23/2012 17:43'!
storeSampleCount: samplesToStore bigEndian: bigEndianFlag on: aBinaryStream
	"Store my samples on the given stream at the current SoundPlayer sampling rate. If bigFlag is true, then each 16-bit sample is stored most-significant byte first (AIFF files), otherwise it is stored least-significant byte first (WAV files). If self isStereo is true, both channels are stored, creating a stereo file. Otherwise, only the left channel is stored, creating a mono file."

	| bufSize stereoBuffer reverseBytes  |
	self reset.
	bufSize _ (2 * self samplingRate rounded) min: samplesToStore.  "two second buffer"
	stereoBuffer _ SoundBuffer newStereoSampleCount: bufSize.
	reverseBytes _ bigEndianFlag ~= Smalltalk isBigEndian.

	'Storing audio...' displayProgressAt: Sensor mousePoint
		from: 0 to: samplesToStore during: [:bar | | remaining out |
			remaining _ samplesToStore.
			[remaining > 0] whileTrue: [
				bar value: samplesToStore - remaining.
				stereoBuffer primFill: 0.  "clear the buffer"
				self playSampleCount: (bufSize min: remaining) into: stereoBuffer startingAt: 1.
				self isStereo
					ifTrue: [out _ stereoBuffer]
					ifFalse: [out _ stereoBuffer extractLeftChannel].
				reverseBytes ifTrue: [out reverseEndianness].
				(aBinaryStream isKindOf: StandardFileStream)
					ifTrue: [  "optimization for files: write sound buffer directly to file"
						aBinaryStream next: (out size // 2) putAll: out startingAt: 1]  "size in words"
					ifFalse: [  "for non-file streams:"
						1 to: out monoSampleCount do: [:i | aBinaryStream int16: (out at: i)]].
				remaining _ remaining - bufSize]].
! !


!Debugger methodsFor: 'initialize' stamp: 'jmv 10/23/2012 18:09'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Sensor currentCursor.
	Sensor currentCursor: Cursor normal.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self lowSpaceChoices, (msgString ifNil: ['']) ]
		ifFalse: [ msgString ].

	errorWasInUIProcess _ ProjectX newProcessIfUIX: interruptedProcess.
	WorldState addDeferredUIMessage: [
		PreDebugWindow open: self label: label message: msg ].
	^self! !

!Debugger methodsFor: 'private' stamp: 'jmv 10/23/2012 17:45'!
resumeProcess
	savedCursor
		ifNotNil: [Sensor currentCursor: savedCursor].
	interruptedProcess isTerminated ifFalse: [
		interruptedProcess resume ].
	"if old process was terminated, just terminate current one"
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	self runningWorld ifNotNil: [ :w | w displayWorld ].
	Smalltalk installLowSpaceWatcher.
	"restart low space handler"
	errorWasInUIProcess == false
		ifFalse: [ Processor terminateActive ]! !


!DisplayScreen methodsFor: 'other' stamp: 'jmv 10/23/2012 17:45'!
newDepth: pixelSize
"
	Display newDepth: 8.
	Display newDepth: 1.
"
	(self supportsDisplayDepth: pixelSize)
		ifFalse:[^self inform:'Display depth ', pixelSize printString, ' is not supported on this system'].
	self newDepthNoRestore: pixelSize.
	self runningWorld ifNotNil: [ :w | w  buildMagnifiedBackgroundImage ].
	self restore.! !

!DisplayScreen methodsFor: 'other' stamp: 'jmv 10/23/2012 17:45'!
restore
	self runningWorld ifNotNil: [ :w | w fullRepaintNeeded ]! !

!DisplayScreen methodsFor: 'other' stamp: 'jmv 10/23/2012 17:45'!
restoreAfter: aBlock
	"Evaluate the block, wait for a mouse click, and then restore the screen."

	aBlock value.
	Sensor waitButton.
	self runningWorld ifNotNil: [ :w | w fullRepaintNeeded ]! !


!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 10/23/2012 17:45'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	Display extent = DisplayScreen actualScreenSize ifTrue: [^ self].
	DisplayScreen startUp.
	self runningWorld ifNotNil: [ :w | w restoreMorphicDisplay ]! !


!Morph methodsFor: 'initialization' stamp: 'jmv 10/23/2012 17:49'!
openInHand
	"Attach the receiver to the current hand in the current morphic world"

	self runningWorld activeHand attachMorph: self! !

!Morph methodsFor: 'initialization' stamp: 'jmv 10/23/2012 17:48'!
openInWorld

	self runningWorld ifNotNil: [ :w | self openInWorld: w ]! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 10/23/2012 17:51'!
resizeMorph
	| handle |
	handle _ HandleMorph new 
				forEachPointDo: [ :newPoint | self morphExtent: newPoint - self morphPositionInWorld].
	self runningWorld activeHand attachMorph: handle.
	handle startStepping! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 10/23/2012 17:51'!
delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| aWorld |
	aWorld _ self world ifNil: [ self runningWorld ].
	"Terminate genie recognition focus"
	"I encountered a case where the hand was nil, so I put in a little 
	protection - raa "
	" This happens when we are in an MVC project and open
	  a morphic window. - BG "
	aWorld ifNotNil: [
		aWorld activeHand
			releaseKeyboardFocus: self;
			releaseMouseFocus: self].
	owner ifNotNil:[ self privateDelete].! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 10/23/2012 17:44'!
request: queryString
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph request: 'What is your favorite color?'"

	^ self
		request: queryString
		initialAnswer: ''
		centerAt: self runningWorld activeHand morphPosition
		onCancelReturn: ''
		acceptOnCR: true
		answerExtent: self defaultAnswerExtent! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 10/23/2012 17:44'!
request: queryString initialAnswer: defaultAnswer 
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph
		request: 'What is your favorite color?'
		initialAnswer: 'red, no blue. Ahhh!!'"

	^ self
		request: queryString
		initialAnswer: defaultAnswer
		centerAt: self runningWorld activeHand morphPosition
		onCancelReturn: ''
		acceptOnCR: true
		answerExtent: self defaultAnswerExtent! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 10/23/2012 17:45'!
request: queryString initialAnswer: defaultAnswer centerAt: aPoint onCancelReturn: returnOnCancel acceptOnCR: acceptBoolean answerExtent: answerExtent
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts.   If the user cancels, answer returnOnCancel."
	"
	FillInTheBlankMorph
		request: 'Type something, then type [Return].'
		initialAnswer: 'yo ho ho!!'
		centerAt: Display center
	"

	| aFillInTheBlankMorph |
	aFillInTheBlankMorph _ self new
		setQuery: queryString
		initialAnswer: defaultAnswer
		answerExtent: answerExtent
		acceptOnCR: acceptBoolean.
	aFillInTheBlankMorph responseUponCancel: returnOnCancel.
	self runningWorld addMorph: aFillInTheBlankMorph centeredNear: aPoint.
	^ aFillInTheBlankMorph getUserResponse
! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 10/23/2012 17:47'!
acceptContents
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| ok prevSelection prevScrollValue |
	prevSelection _ self editor selectionInterval copy.
	prevScrollValue _ owner verticalScrollBar value.
	(self canDiscardEdits and: [(self hasProperty: #alwaysAccept) not]) 
		ifTrue: [^self flash].
	self hasEditingConflicts 
		ifTrue: [
			(self confirm: 
'Caution!! Contents were saved
elsewhere since you started
editing them here.  Accept anyway?' ) 
					ifFalse: [^self flash]].
	ok _ model acceptContentsFrom: owner.
	ok == true
		ifTrue: [ model refetch ].

	"sps 8/13/2001 22:41: restore selection and scroll info"
	self flag: #jmvVer2.	"Check this. Do we need the error handler? Consider explicitly the case where no world?"
	["During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the codeholding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
		ok
			ifTrue: [
				self editor selectFrom: prevSelection first to: prevSelection last.
				WorldState addDeferredUIMessage: [
					self world activeHand newKeyboardFocus: self.
					owner setScrollDeltas.
					owner verticalScrollBar setValue: prevScrollValue ]]
	] on: Error do: nil! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 10/23/2012 17:58'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [:m | m isKindOf: UpdatingMenuItemMorph]) 
		do: [:m | m updateContents].
	self runningWorld addMorphFront: self.
	self 
		positionAt: aPoint
		forHand: hand
		relativeTo: (selectedItem ifNil: [self items first]).
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [hand newKeyboardFocus: self].
	evt := hand lastEvent.
	(evt isKeyboard or: [evt isMouse and: [evt anyButtonPressed not]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt]! !

!MenuMorph methodsFor: 'control' stamp: 'jmv 10/23/2012 17:58'!
popUpInWorld
	"Present this menu in the current World"

	^ self popUpInWorld: self runningWorld! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 10/23/2012 17:58'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w oldFocus actHand |
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneSubCycle.
	self	
		popUpAt: actHand morphPosition
		forHand: actHand 
		allowKeyboard: allowKeyboardControl.
	self isModalInvokationDone: false.
	[self isInWorld & self isModalInvokationDone not] whileTrue: [w doOneSubCycle].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ self modalSelection! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 10/23/2012 17:53'!
displayAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	self runningWorld ifNotNil: [ :w |
		w addMorph: self centeredNear: aPoint.
		self world displayWorld.  "show myself"
		].
	aBlock value.
	self delete! !

!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 10/23/2012 17:56'!
informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString _ titleMorph submorphs first.
	self visible: false.
	w _ self world ifNil: [ self runningWorld ].
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString width + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w displayWorld		 "show myself"
	]. 
	self delete.
	w displayWorld! !

!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 10/23/2012 17:57'!
invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	|actHand w oldFocus |
	self flag: #bob.		"is <aPoint> global or local?"
	self flag: #arNote.	"<aPoint> is local to aWorld"
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneSubCycle.
	self
		popUpAt: aPoint
		forHand: actHand
		allowKeyboard: aBoolean.
	done _ false.
	[self isInWorld & done not] whileTrue: [w doOneSubCycle].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ mvcSelection ! !


!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 10/23/2012 17:51'!
monitor: anObjectExplorerWrapper
	"Start stepping and watching the given wrapper for changes."
	anObjectExplorerWrapper ifNil: [ ^self ].
	self monitorList at: anObjectExplorerWrapper put: anObjectExplorerWrapper asString.
	self runningWorld startStepping: self at: Time millisecondClockValue selector: #step arguments: #() stepTime: 200! !

!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 10/23/2012 17:51'!
step
	"If there's anything in my monitor list, see if the strings have changed."
	| string changes |
	changes _ false.
	self monitorList keysAndValuesDo: [ :k :v |
		k ifNotNil: [
			k refresh.
			(string _ k asString) ~= v ifTrue: [ self monitorList at: k put: string. changes _ true ].
		]
	].
	changes ifTrue: [ | sel |
		sel _ currentSelection.
		self changed: #getList.
		self noteNewSelection: sel.
	].
	
	self monitorList isEmpty ifTrue: [
		self runningWorld stopStepping: self selector: #step ]! !

!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 10/23/2012 17:51'!
stopMonitoring
	monitorList _ nil.
	self runningWorld stopStepping: self selector: #step! !


!PasteUpMorph methodsFor: 'caching' stamp: 'jmv 10/23/2012 17:26'!
releaseCachedState
	super releaseCachedState.
	backgroundImage _ nil.
	self isWorldMorph ifTrue: [
		self cleanseStepList.
		worldState canvas: nil ]! !


!PasteUpMorph class methodsFor: 'system startup' stamp: 'jmv 10/23/2012 17:28'!
shutDown

	"Should use some other way to find relevant instances"
	self flag: #jmvVer2.
	self allInstancesDo: [ :each |
		each releaseCachedState ]! !

!PasteUpMorph class methodsFor: 'system startup' stamp: 'jmv 10/23/2012 17:30'!
startUp

	"Should use some other way to find relevant instances"
	self flag: #jmvVer2.
	self allInstancesDo: [ :each |
		each buildMagnifiedBackgroundImage.
		each isWorldMorph ifTrue: [
			each restoreMorphicDisplay.

			"5.8 series of Mac VM seem to ignore the first call to primitive 127.
			This is a workaround for that."
			"Display forceToScreen: (0@0 extent: 1@1)."
			"Now it seems that VM will not be widely used... If Display artifacts appear with newer Cog Mac VMs,
			reitroduce it, maybe checking VM version"
		]
	]! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 10/23/2012 17:43'!
startUpWithoutKeyboard
	"Display and make a selection from the receiver as long as the button  is pressed. Answer the current selection.  Do not allow keyboard input into the menu"
	
	^ self startUpWithCaption: nil at: self runningWorld activeHand morphPosition allowKeyboard: false! !


!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 10/23/2012 18:04'!
defaultAction

	| delta textFrame barFrame outerFrame result range width w1 f h textWidth textForm innerBarFrame |
	f _ StrikeFont default.
	h _ f height * 3//2.
	textWidth _ (f widthOfString: progressTitle) + h.
	width _ 150 max: textWidth.
	
	textForm _ Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).
			
	barFrame _ Rectangle center: aPoint extent: width@h.
	textFrame _ 0@0 extent: width@h.
	textFrame _ textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + (0@2).
	outerFrame _ barFrame merge: textFrame.
	delta _ outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame _ barFrame translatedBy: delta.
	textFrame _ textFrame translatedBy: delta.
	outerFrame _ outerFrame translatedBy: delta.
	range _ maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame _ barFrame insetBy: 2.
	result _ workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal 
			ifNotNil: [ currentVal _ barVal ]
			ifNil: [		
				currentVal _ currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal _ minVal ]].
		w1 _ ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		textForm displayAt: textFrame topLeft.
		Display fillBlack: barFrame.
		Display fillWhite: innerBarFrame.
		Display fillGray: (barFrame topLeft + (2@2) extent: w1@17) ].
	"Not pretty at all!!"
	self runningWorld ifNotNil: [ :w | w fullRepaintNeeded ].
	self resume: result! !


!Rectangle methodsFor: 'transforming' stamp: 'jmv 10/23/2012 17:58'!
newRectFrom: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow aHand delay |
	delay _ Delay forMilliseconds: 10.
	buttonStart _ buttonNow _ Sensor anyButtonPressed.
	rect _ self.
	Display border: rect width: 2 rule: Form reverse fillColor: Color gray.
	[buttonNow == buttonStart] whileTrue: 
		[delay wait.
		buttonNow _ Sensor anyButtonPressed.
		newRect _ newRectBlock value: rect.
		newRect = rect ifFalse:
			[Display border: rect width: 2 rule: Form reverse fillColor: Color gray.
			Display border: newRect width: 2 rule: Form reverse fillColor: Color gray.
			rect _ newRect]].
	Display border: rect width: 2 rule: Form reverse fillColor: Color gray.
	" pay the price for reading the sensor directly ; get this party started "
	aHand _ self runningWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent.
	^ rect! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 10/23/2012 17:59'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	ClassicTheme beCurrent.
	PasteUpMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	FormCanvas clearFormsCache.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
"	ColorPickerMorph class removeSelector: #buildEyedropperIcon."
	CursorWithAlpha class removeSelector: #buildBiggerNormal.
	Theme removeSelector: #miscellaneousIcons.
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont removeMostFonts.
	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	"????
	Smalltalk organization removeCategoriesMatching: 'Signal Processing*'.
	SystemOrganization removeSystemCategory: 'LinearAlgebra'.
	Smalltalk organization removeCategoriesMatching: 'Sound-*'
	"

	Beeper setDefault: nil.
	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(ZipConstants GZipConstants ZipFileConstants ChronologyConstants SpaceTally).
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSorter zapAllChangeSets.
	Smalltalk garbageCollect.


	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'jmv 10/23/2012 18:10'!
open: model label: aString message: messageString
	|  window extentToUse |
	window _ self new.
	window
		model: model;
		buildMorphicWindowMessage: messageString.
	aString ifNotNil: [ window setLabel: aString ].
	extentToUse _ 560 @ 300.
	"nice and wide to show plenty of the error msg"
	 window openInWorld: self runningWorld extent: extentToUse! !


!Theme class methodsFor: 'class initialization' stamp: 'jmv 10/23/2012 18:03'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 initialize.
	self runningWorld backgroundImage ifNil: [ self runningWorld color: CurrentTheme background ].
	SystemWindow initialize.
	FormCanvas clearFormsCache.
	Taskbar reset; initialize.
	self runningWorld restoreMorphicDisplay.
	
	^ CurrentTheme! !


!Utilities class methodsFor: 'common requests' stamp: 'jmv 10/23/2012 18:04'!
saveScreenshot
	"Make a screenshot of the world and save it to a file"

	"SampledSound playSoundNamed: 'camera'."
	PNGReadWriter
		putForm: (self runningWorld imageForm: 32)
		onFileNamed:
			(FileDirectory default
				nextNameFor: 'CuisScreen'
				extension: 'png')! !

!Utilities class methodsFor: 'closure support' stamp: 'jmv 10/23/2012 18:03'!
postRecompileCleanup	"Utilities postRecompileCleanup"
	"Cleanup after loading closure bootstrap"
	"Before doing this, please start a new UI process (for example, by hitting alt-period and closing the debugger)."
	| unboundMethods contexts |
	self runningWorld ifNotNil: [ :w | w removeAllKnownFailing ].
	ProcessorScheduler startUp.
	WeakArray restartFinalizationProcess.
	MethodChangeRecord allInstancesDo:[:x| x noteNewMethod: nil].
	Smalltalk cleanOutUndeclared.
	Delay startTimerEventLoop.
	EventSensor install.
	WorldState allInstancesDo:[:ws| ws convertAlarms; convertStepList].
	Workspace allInstancesDo:[:ws| ws initializeBindings].
	Smalltalk garbageCollect.
	Smalltalk
		at: #DebuggerMethodMap
		ifPresent: [ :dmm | dmm voidMapCache ].
	Smalltalk garbageCollect.
	unboundMethods _ CompiledMethod unboundMethods.
	unboundMethods notEmpty ifTrue: [
		unboundMethods inspectWithLabel: 'Unbound Methods'].
	Smalltalk at: #BlockContext ifPresent: [ :bc |
		contexts _ bc allInstances.
		contexts ifNotEmpty: [
			contexts inspect. 
			self inform: 'There are left-over BlockContexts'.
			^self ]].
	unboundMethods isEmpty ifTrue:[
		self inform:'Congratulations - The bootstrap is now complete.'.
	]! !

!methodRemoval: SystemWindow #openInWorldExtent:!
SystemWindow removeSelector: #openInWorldExtent:!
!methodRemoval: PasteUpMorph #install!
PasteUpMorph removeSelector: #install!

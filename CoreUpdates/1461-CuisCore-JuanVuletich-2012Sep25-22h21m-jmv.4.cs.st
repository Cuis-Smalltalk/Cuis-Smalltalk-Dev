'From Cuis 4.0 of 21 April 2012 [latest update: #1460] on 25 September 2012 at 10:57:40 pm'!
!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent activeHand '
	classVariableNames: 'DeferredUIMessages MaxCycleLapse '
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!MenuMorph methodsFor: 'private' stamp: 'jmv 9/25/2012 22:30'!
positionAt: aPoint forHand: hand relativeTo: aMenuItem
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| i yOffset sub delta |
	self adjustSubmorphsLayout.
	i _ 0.
	yOffset _ 0.
	[(sub _ self submorphs at: (i _ i + 1)) == aMenuItem]
		whileFalse: [ yOffset _ yOffset + sub morphHeight ].

	self morphPosition: aPoint - (2 @ (yOffset + 8)).

	"If it doesn't fit, show it to the left, not to the right of the hand."
	self morphBoundsInWorld right > owner world morphBoundsInWorld right
		ifTrue: [
			self moveRight: aPoint x + 1].

	"Make sure that the menu fits in the world."
	delta _ self morphBoundsInWorld amountToTranslateWithin:
		(owner world morphBoundsInWorld withHeight: ((owner world morphExtentInWorld y - 18) max: (hand morphPosition y) + 1)).
	delta = (0 @ 0) ifFalse: [ self morphPosition: self morphPosition + delta ]! !


!PasteUpMorph methodsFor: 'accessing' stamp: 'jmv 9/25/2012 22:40'!
activeHand
	"Answer the currently active hand, if any..."
	^worldState activeHand! !


!WorldState methodsFor: 'hands' stamp: 'jmv 9/25/2012 22:39'!
activeHand
	^activeHand! !

!WorldState methodsFor: 'initialization' stamp: 'jmv 9/25/2012 22:38'!
setDefaultActiveHand
	activeHand _ hands first! !


!AbstractSound methodsFor: 'file i/o' stamp: 'jmv 9/25/2012 22:21'!
storeSampleCount: samplesToStore bigEndian: bigEndianFlag on: aBinaryStream
	"Store my samples on the given stream at the current SoundPlayer sampling rate. If bigFlag is true, then each 16-bit sample is stored most-significant byte first (AIFF files), otherwise it is stored least-significant byte first (WAV files). If self isStereo is true, both channels are stored, creating a stereo file. Otherwise, only the left channel is stored, creating a mono file."

	| bufSize stereoBuffer reverseBytes  |
	self reset.
	bufSize _ (2 * self samplingRate rounded) min: samplesToStore.  "two second buffer"
	stereoBuffer _ SoundBuffer newStereoSampleCount: bufSize.
	reverseBytes _ bigEndianFlag ~= Smalltalk isBigEndian.

	'Storing audio...' displayProgressAt: self currentWorld activeHand morphPosition
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


!Debugger methodsFor: 'private' stamp: 'jmv 9/25/2012 22:45'!
resumeProcess
	savedCursor
		ifNotNil: [Sensor currentCursor: savedCursor].
	interruptedProcess isTerminated ifFalse: [
		errorWasInUIProcess
					ifTrue: [ProjectX resumeProcessX: interruptedProcess]
					ifFalse: [interruptedProcess resume]].
	"if old process was terminated, just terminate current one"
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	self currentWorld ifNotNil: [ :w | w displayWorld ].
	Smalltalk installLowSpaceWatcher.
	"restart low space handler"
	errorWasInUIProcess == false
		ifFalse: [Processor terminateActive]! !


!DisplayScreen methodsFor: 'other' stamp: 'jmv 9/25/2012 22:46'!
newDepth: pixelSize
"
	Display newDepth: 8.
	Display newDepth: 1.
"
	(self supportsDisplayDepth: pixelSize)
		ifFalse:[^self inform:'Display depth ', pixelSize printString, ' is not supported on this system'].
	self newDepthNoRestore: pixelSize.
	self currentWorld ifNotNil: [ :w | w  buildMagnifiedBackgroundImage ].
	self restore.! !

!DisplayScreen methodsFor: 'other' stamp: 'jmv 9/25/2012 22:46'!
restore
	self currentWorld ifNotNil: [ :w | w fullRepaintNeeded ]! !

!DisplayScreen methodsFor: 'other' stamp: 'jmv 9/25/2012 22:46'!
restoreAfter: aBlock
	"Evaluate the block, wait for a mouse click, and then restore the screen."

	aBlock value.
	Sensor waitButton.
	self currentWorld ifNotNil: [ :w | w fullRepaintNeeded ]! !


!DisplayScreen class methodsFor: 'display box access' stamp: 'jmv 9/25/2012 22:46'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	Display extent = DisplayScreen actualScreenSize ifTrue: [^ self].
	DisplayScreen startUp.
	self currentWorld ifNotNil: [ :w | w restoreMorphicDisplay ]! !


!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 9/25/2012 22:47'!
testLayout1
	"
	self new testLayout1
	"
	| pane row1 row2 row3 r1c1 r1c2 r1c3 r1c4 r1c5 r2c1 r2c2 r2c3 r3c1 r3c2 r3c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row1 _ LayoutMorph newRow separation: 5.
	row1 color: Color red;
		addMorph: (r1c1 _ BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 10);
		addMorph: (r1c2 _ BorderedRectMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r1c3 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r1c4 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.15);
		addMorph: (r1c5 _ BorderedRectMorph new color: (Color h: 60 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 fixedHeight: 20).
	pane addMorph: row1 layoutSpec: LayoutSpec useAll.
	row2 _ LayoutMorph newRow separation: 5.
	row2 color: Color red;
		addMorph: (r2c1 _ BorderedRectMorph new color: Color blue)
			layoutSpec: (LayoutSpec proportionalWidth: 0.8);
		addMorph: (r2c2 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.4);
		addMorph: (r2c3 _ BorderedRectMorph new color: (Color h: 30 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.2).
	pane addMorph: row2 layoutSpec: LayoutSpec useAll.
	row3 _ LayoutMorph newRow separation: 5.
	row3 color: Color red;
		addMorph: (r3c1 _ BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8);
		addMorph: (r3c2 _ BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40);
		addMorph: (r3c3 _ BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row3 layoutSpec: (LayoutSpec fixedHeight: 60).
	pane openInWorld; morphExtent: 408@300.
	self currentWorld doOneCycleNow.

	self assert: row1 morphWidth = (pane morphWidth - 10).
	self assert: r1c1 morphWidth class == SmallInteger.
	self assert: r1c1 morphHeight class == SmallInteger.
	self assert: r1c1 morphWidth = 10.
	self assert: r1c1 morphHeight = (row1 morphHeight - 10).
	self assert: r1c2 morphWidth = 200.
	self assert: r1c2 morphHeight = (row1 morphHeight - 10).
	self assert: r1c3 morphWidth = (r1c2 morphWidth / 0.8 * 0.4) rounded.
	self assert: r1c3 morphHeight = (row1 morphHeight - 10).
	self assert: r1c4 morphWidth = (r1c2 morphWidth / 0.8 * 0.15) rounded.
	self assert: r1c4 morphHeight = (row1 morphHeight - 10).
	self assert: r1c5 morphWidth = 20.
	self assert: r1c5 morphHeight = 20.

	self assert: row2 morphWidth = (pane morphWidth - 10).
	self assert: r2c1 morphWidth = 216.
	self assert: r2c1 morphHeight = (row2 morphHeight - 10).
	self assert: r2c2 morphWidth = (r2c1 morphWidth / 0.8 * 0.4) rounded.
	self assert: r2c2 morphHeight = (row2 morphHeight - 10).
	self assert: r2c3 morphWidth = (r2c1 morphWidth / 0.8 * 0.2) rounded.
	self assert: r2c3 morphHeight = (row2 morphHeight - 10).

	self assert: row3 morphWidth = (pane morphWidth - 10).
	self assert: row3 morphHeight = 60.
	self assert: r3c1 morphWidth = 20.
	self assert: r3c1 morphHeight = (row3 morphHeight - 10 * 0.8) rounded.
	self assert: r3c2 morphWidth = (row3 morphWidth - 10 - 20 - 10 - 30 * 0.5) rounded.
	self assert: r3c2 morphHeight = 40.
	self assert: r3c3 morphWidth = 30.
	self assert: r3c3 morphHeight = (row3 morphHeight - 10).

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 9/25/2012 22:47'!
testLayout2
	"
	self new testLayout2
	"
	| pane row c1 c2 c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: (c1 _ BorderedRectMorph new color: (Color h: 120 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 20 proportionalHeight: 0.8 minorDirectionPadding: #bottom);
		addMorph: (c2 _ BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.8 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (c3 _ BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 0.7 minorDirectionPadding: #center).
	pane addMorph: row layoutSpec: (LayoutSpec proportionalHeight: 0.9).
	pane openInWorld; morphExtent: 400@300.
	self currentWorld doOneCycleNow.

	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = (pane morphHeight - 10 * 0.9) rounded.
	self assert: c1 morphBoundsInWorld bottom = (row morphBoundsInWorld bottom - 5) description: 'Should be at bottom'.
	self assert: c1 morphWidth = 20.
	self assert: c1 morphHeight = (row morphHeight - 10 * 0.8) rounded.
	self assert: c2 morphBoundsInWorld top = (row morphBoundsInWorld top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = 256.
	self assert: c2 morphHeight = 40.
	self assert: ((c3 morphBoundsInWorld top - row morphBoundsInWorld top) - (row morphBoundsInWorld bottom - c3 morphBoundsInWorld bottom)) abs < 2 description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (c1 morphHeight / 0.8 * 0.7) rounded.

	pane delete! !

!LayoutMorphTest methodsFor: 'tests' stamp: 'jmv 9/25/2012 22:47'!
testLayout3
	"
	self new testLayout3
	"
	| pane row innerRow i1 i2 i3 c2 c3 |
	pane _ LayoutMorph newColumn separation: 5.
	pane color: Color red.
	row _ LayoutMorph newRow separation: 5.
	innerRow _ LayoutMorph newRow color: Color red;  separation: 5.
	innerRow
		addMorph: (i1 _ RectangleLikeMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i2 _ RectangleLikeMorph new)
			layoutSpec: (LayoutSpec fixedWidth: 10 fixedHeight: 10);
		addMorph: (i3 _ RectangleLikeMorph new)
			layoutSpec: (LayoutSpec proportionalWidth: 1.0 fixedHeight: 10).
	row
		color: (Color h: 270 s: 0.2 v: 0.6);
		addMorph: innerRow
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 30 minorDirectionPadding: #center);
		addMorph: (c2 _ BorderedRectMorph new color: (Color h: 90 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec proportionalWidth: 0.5 fixedHeight: 40 minorDirectionPadding: #top);
		addMorph: (c3 _ BorderedRectMorph new color: (Color h: 150 s: 0.6 v: 0.6))
			layoutSpec: (LayoutSpec fixedWidth: 30 proportionalHeight: 1.0).
	pane addMorph: row layoutSpec: (LayoutSpec fixedHeight: 200).
	pane openInWorld; morphExtent: 400@300.
	self currentWorld doOneCycleNow.

	self assert: row morphBoundsInWorld left = (pane morphBoundsInWorld left + 5).
	self assert: row morphWidth = (pane morphWidth - 10).
	self assert: row morphHeight = 200.
	self assert: innerRow morphBoundsInWorld left = (row morphBoundsInWorld left + 5).
	self assert: (innerRow morphBoundsInWorld top - row morphBoundsInWorld top) = (row morphBoundsInWorld bottom - innerRow morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: innerRow morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: innerRow morphHeight = 30.

	self assert: i1 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 5).
	self assert: (i1 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i1 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i1 morphWidth = 10.
	self assert: i1 morphHeight = 10.
	self assert: i2 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 20).
	self assert: (i2 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i2 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i2 morphWidth = 10.
	self assert: i2 morphHeight = 10.
	self assert: i3 morphBoundsInWorld left = (innerRow morphBoundsInWorld left + 35).
	self assert: (i3 morphBoundsInWorld top - innerRow morphBoundsInWorld top) = (innerRow morphBoundsInWorld bottom - i3 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: i3 morphWidth = (innerRow morphWidth - 40).
	self assert: i3 morphHeight = 10.

	self assert: c2 morphBoundsInWorld top = (row morphBoundsInWorld top + 5) description: 'Should be at top'.
	self assert: c2 morphWidth = (pane morphWidth - 10 - 10 - 30 - 10 * 0.5) rounded.
	self assert: c2 morphHeight = 40.
	self assert: (c3 morphBoundsInWorld top - row morphBoundsInWorld top) = (row morphBoundsInWorld bottom - c3 morphBoundsInWorld bottom) description: 'Should be centered'.
	self assert: c3 morphWidth = 30.
	self assert: c3 morphHeight = (row morphHeight - 10).

	pane delete! !


!Morph methodsFor: 'focus handling' stamp: 'jmv 9/25/2012 22:31'!
hasKeyboardFocus

	self world ifNotNil: [ :w |
		w activeHand ifNotNil: [ :h |
			^ h keyboardFocus == self ]].
	^ false! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/25/2012 22:26'!
showBalloon: msgString
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."
	| w |
	self showBalloon: msgString hand: ((w _ self world) ifNotNil: [ w activeHand ])! !

!Morph methodsFor: 'initialization' stamp: 'jmv 9/25/2012 22:25'!
openInHand
	"Attach the receiver to the current hand in the current morphic world"

	self currentWorld activeHand attachMorph: self! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 9/25/2012 22:25'!
resizeMorph
	| handle |
	handle := HandleMorph new 
				forEachPointDo: [:newPoint | self morphExtent: newPoint - self morphPositionInWorld].
	self currentWorld activeHand attachMorph: handle.
	handle startStepping! !

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/25/2012 22:25'!
delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| aWorld |
	aWorld := self world ifNil: [ self currentWorld ].
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


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 9/25/2012 22:21'!
request: queryString
	"Create an instance of me whose question is queryString. Invoke it centered at the cursor, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph request: 'What is your favorite color?'"

	^ self
		request: queryString
		initialAnswer: ''
		centerAt: self currentWorld activeHand morphPosition
		onCancelReturn: ''
		acceptOnCR: true
		answerExtent: self defaultAnswerExtent! !

!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 9/25/2012 22:21'!
request: queryString initialAnswer: defaultAnswer 
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."
	"FillInTheBlankMorph
		request: 'What is your favorite color?'
		initialAnswer: 'red, no blue. Ahhh!!'"

	^ self
		request: queryString
		initialAnswer: defaultAnswer
		centerAt: self currentWorld activeHand morphPosition
		onCancelReturn: ''
		acceptOnCR: true
		answerExtent: self defaultAnswerExtent! !


!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/25/2012 22:46'!
nextFocusMorph

	^(keyboardFocus ifNil: [ self world ])
		previousMorphThat: [ :m |
			m handlesKeyboard and: [ m isReallyVisible ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/25/2012 22:46'!
nextFocusWindow

	^(SystemWindow topWindow ifNil: [ self world ])
		nextMorphThat: [ :m | 
			(m is: #SystemWindow) and: [ m isReallyVisible ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/25/2012 22:46'!
previousFocusMorph
	^ (keyboardFocus ifNil: [ self world ])
		nextMorphThat: [ :m | 
			m handlesKeyboard and: [ m isReallyVisible ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/25/2012 22:46'!
previousFocusWindow

	^ (SystemWindow topWindow ifNil: [ self world ])
		previousMorphThat: [ :m |
			(m is: #SystemWindow) and: [ m isReallyVisible ]]! !

!HandMorph methodsFor: 'objects from disk' stamp: 'jmv 9/25/2012 22:22'!
objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	"owned by the project"
"	(refStrm project world hands includes: self) ifTrue: [
		^ self].	"

	self flag: #jmvVer2.

	dp _ DiskProxy global: #World selector: #activeHand args: #().
	refStrm replace: self with: dp.
	^ dp
	"Note, when this file is loaded in an MVC project, this will return nil.  The MenuItemMorph that has this in a field will have that item not work.  Maybe warn the user at load time?"! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 9/25/2012 22:23'!
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
	
	["During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the codeholding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
		ok
			ifTrue: [
				self editor selectFrom: prevSelection first to: prevSelection last.
				WorldState addDeferredUIMessage: [
						self currentWorld activeHand newKeyboardFocus: self.
						owner setScrollDeltas.
						owner verticalScrollBar setValue: prevScrollValue ]]
	] on: Error do: nil! !


!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 9/25/2012 22:55'!
magnifiedForm
	| srcRect form neededExtent |
	lastPos _ self sourcePoint.
	srcRect _ self sourceRectFrom: lastPos.
	((srcRect intersects: self morphBoundsInWorld) and: [ RecursionLock == nil ])
		ifTrue: [
			RecursionLock _ self.
			"try to reuse form if appropriate"
			auxCanvas _ (auxCanvas notNil and: [ auxCanvas extent = srcExtent ])
				ifTrue: [
					"Just in case we go out of the Display"
					srcRect origin > (0@0) ifFalse: [
						auxCanvas form fillBlack ].
					FormCanvas on: auxCanvas form over: srcRect ]
				ifFalse: [ FormCanvas depth: 32 over: srcRect ].
			self world drawOn: auxCanvas.
			self world drawSubmorphsOn: auxCanvas.
			form _ auxCanvas form.
			RecursionLock _ nil]
		ifFalse: [
			"cheaper method if the source is not occluded"
			form _ Display copy: srcRect].
	"smooth if non-integer scale"
	neededExtent _ (srcExtent * magnification ) truncated.
	(magnifiedForm isNil or: [ magnifiedForm extent ~=  neededExtent ])
		ifTrue: [ magnifiedForm _ Form extent: neededExtent depth: 32 ].
	(WarpBlt toForm: magnifiedForm)
		sourceForm: form;
		colorMap: (form colormapIfNeededFor: magnifiedForm);
		cellSize: (magnification isInteger ifTrue: [1] ifFalse: [2]);  "installs a new colormap if cellSize > 1"
		combinationRule: 3;
		copyQuad: form boundingBox innerCorners toRect: magnifiedForm boundingBox.
	^magnifiedForm.! !

!MagnifierMorph methodsFor: 'magnifying' stamp: 'jmv 9/25/2012 22:24'!
sourcePoint
	"If we are being dragged use our center, otherwise use pointer position"
	^ (trackPointer not or: [owner notNil and: [owner is: #HandMorph]])
		ifTrue: [ self morphBoundsInWorld center ]
		ifFalse: [ self world activeHand morphPosition ]! !


!MenuMorph methodsFor: 'control' stamp: 'jmv 9/25/2012 22:30'!
popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [:m | m isKindOf: UpdatingMenuItemMorph]) 
		do: [:m | m updateContents].
	self currentWorld addMorphFront: self.
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

!MenuMorph methodsFor: 'control' stamp: 'jmv 9/25/2012 22:30'!
popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	Theme current decorateMenu: self.
	(self submorphs select: [:m | m isKindOf: UpdatingMenuItemMorph]) 
		do: [:m | m updateContents].
	aWorld addMorphFront: self.
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

!MenuMorph methodsFor: 'events' stamp: 'jmv 9/25/2012 22:24'!
keyStroke: aKeyboardEvent 
	| matchString char asc selectable help |
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self activeHand.
	char := aKeyboardEvent keyCharacter.
	asc := char asciiValue.
	aKeyboardEvent isReturnKey
		ifTrue: [
			selectedItem ifNotNil: 
					[selectedItem hasSubMenu 
						ifTrue: [
							aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
							^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
						ifFalse: 
							["self delete."

							^selectedItem invokeWithEvent: aKeyboardEvent]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: aKeyboardEvent].
			^self].
	asc = 27 
		ifTrue: 
			["escape key"

			self valueOfProperty: #matchString
				ifPresentDo: 
					[:str | 
					str isEmpty 
						ifFalse: 
							["If filtered, first ESC removes filter"

							self setProperty: #matchString toValue: String new.
							self selectItem: nil event: aKeyboardEvent.
							^self displayFiltered: aKeyboardEvent]].
			"If a stand-alone menu, just delete it"
			popUpOwner ifNil: [^self delete].
			"If a sub-menu, then deselect, and return focus to outer menu"
			self selectItem: nil event: aKeyboardEvent.
			aKeyboardEvent hand newMouseFocus: popUpOwner owner.
			^aKeyboardEvent hand newKeyboardFocus: popUpOwner owner].
	(asc = 28 or: [asc = 29]) 
		ifTrue: 
			["left or right arrow key"

			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: 
					[aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]].
	asc = 30 ifTrue: [^self moveSelectionDown: -1 event: aKeyboardEvent].	"up arrow key"
	asc = 31 ifTrue: [^self moveSelectionDown: 1 event: aKeyboardEvent].	"down arrow key"
	asc = 11 ifTrue: [^self moveSelectionDown: -5 event: aKeyboardEvent].	"page up key"
	asc = 12 ifTrue: [^self moveSelectionDown: 5 event: aKeyboardEvent].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsentPut: [String new].
	matchString := char = Character backspace 
				ifTrue: 
					[matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: aKeyboardEvent keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: aKeyboardEvent.
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self world activeHand.
! !

!MenuMorph methodsFor: 'menu' stamp: 'jmv 9/25/2012 22:30'!
sightTarget: event 
	| bullseye menu newTarget |
	owner
		ifNil: [^ self ].
	bullseye _ Point fromUserWithCursor: Cursor target.
	self world activeHand morphPosition: bullseye.
	menu _ CustomMenu new.
	(owner morphsAt: bullseye) do: [ :m |
		menu add: m printString action: m ].
	menu title: self printString, ' targets... '.
	newTarget _ menu startUp.
	newTarget
		ifNil: [^ self].
	self target: newTarget! !

!MenuMorph methodsFor: 'modal control' stamp: 'jmv 9/25/2012 22:29'!
invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w oldFocus actHand |
	w _ self currentWorld.
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


!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 9/25/2012 22:32'!
invokeWorldMenu
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed"

	| menu |
	menu _ (TheWorldMenu new 
		world: self
		hand: self activeHand) buildWorldMenu.
	menu addTitle: Preferences desktopMenuTitle.
	menu popUpInWorld: self! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 9/25/2012 22:40'!
install
	owner := nil.	"since we may have been inside another world previously"
	worldState setDefaultActiveHand.
	submorphs do: [:ss | ss owner ifNil: [ss privateOwner: self]].
	"Transcript that was in outPointers and then got deleted."
	self viewBox: Display boundingBox.
	Sensor flushAllButDandDEvents.
	worldState handsDo: [:h | h initForEvents].
	self borderWidth: 0.	"default"
	SystemWindow noteTopWindowIn: self.
	self displayWorldSafely! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 9/25/2012 22:32'!
startUpWithoutKeyboard
	"Display and make a selection from the receiver as long as the button  is pressed. Answer the current selection.  Do not allow keyboard input into the menu"
	
	^ self startUpWithCaption: nil at: self currentWorld activeHand morphPosition allowKeyboard: false! !


!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 9/25/2012 22:49'!
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
	self currentWorld ifNotNil: [ :w | w fullRepaintNeeded ].
	self resume: result! !


!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 9/25/2012 22:49'!
interruptNameX: labelString
	"Create a Notifier on the active scheduling process with the given label."
	| preemptedProcess projectProcess |
	
	self currentWorld activeHand interrupted.
	projectProcess _ ProjectX uiProcessX.	"we still need the accessor for a while"
	preemptedProcess _ Processor preemptedProcess.
	"Only debug preempted process if its priority is >= projectProcess' priority"
	preemptedProcess priority < projectProcess priority ifTrue:[
		projectProcess suspend.
		preemptedProcess _ projectProcess.
	] ifFalse:[
		preemptedProcess suspend.
	].
	Debugger openInterrupt: labelString onProcess: preemptedProcess
! !


!Rectangle methodsFor: 'transforming' stamp: 'jmv 9/25/2012 22:26'!
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
	aHand _ self currentWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent.
	^ rect! !


!ReferenceStreamTest methodsFor: 'testing' stamp: 'jmv 9/25/2012 22:42'!
testDiskProxy
	"
	ReferenceStreamTest new testDiskProxy
	"
	| newInstance oldInstance |
	self flag: #jmvVer2.
	oldInstance _ { Smalltalk . Display . Morph}.
	newInstance _ ReferenceStream unStream: (ReferenceStream streamedRepresentationOf: oldInstance).
	1 to: oldInstance size do: [ :i |
		self assert: (newInstance at: i) == (oldInstance at: i) ]! !


!SmartRefStreamTest methodsFor: 'testing' stamp: 'jmv 9/25/2012 22:42'!
testDiskProxy
	"
	SmartRefStreamTest new testDiskProxy
	"
	| newInstance oldInstance |
	self flag: #jmvVer2.
	oldInstance _ { Smalltalk . Display . Morph}.
	newInstance _ SmartRefStream unStream: (SmartRefStream streamedRepresentationOf: oldInstance).
	1 to: oldInstance size do: [ :i |
		self assert: (newInstance at: i) == (oldInstance at: i) ]! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 9/25/2012 22:51'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	ClassicTheme beCurrent.
	self currentWorld ifNotNil: [ :w | w backgroundImageData: nil ].
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

	self currentWorld ifNotNil: [ :w | w  submorphsDo: [ :a | a delete ]].
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


!SystemDictionaryTest methodsFor: 'testing' stamp: 'jmv 9/25/2012 22:51'!
testPointersToEachIn
	"
	SystemDictionaryTest new testPointersToEachIn
	"
	| p1 p2 |
	p1 _ (Smalltalk pointersTo: self currentWorld).
	p2 _ (Smalltalk pointersToEachIn: {self currentWorld}) first.
	self assert: p1 = p2! !


!SystemWindow methodsFor: 'top window' stamp: 'jmv 9/25/2012 22:33'!
passivate
	"Make me unable to respond to mouse and keyboard"

	| focus h |
	h _ self world activeHand.
	focus _ h keyboardFocus.
	focus ifNotNil: [
		(focus ownerThatIsA: SystemWindow) == self
			ifTrue: [ h releaseKeyboardFocus ]].

	self redrawNeeded! !


!Taskbar methodsFor: 'stepping' stamp: 'jmv 9/25/2012 22:51'!
step

	"My dimensions are constrained live."
	| r |
	r _ self world morphBoundsInWorld.
	r _ r left @ (r bottom -18) extent: r width@18.
	self morphBoundsInWorld = r ifFalse: [
		self morphBoundsInWorld: r]! !


!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 9/25/2012 22:51'!
testClassShow
	taskbar class show.
	self should: [ taskbar isInWorld ].
	self currentWorld removeMorph: taskbar! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 9/25/2012 22:51'!
testVerticalAlignment
	
	self should: [ taskbar morphBoundsInWorld bottom = self currentWorld morphBoundsInWorld bottom ]! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 9/25/2012 22:52'!
testWidth
	
	self should: [ taskbar morphWidth = self currentWorld morphWidth ]! !


!TextEditorTest methodsFor: 'as yet unclassified' stamp: 'jmv 9/25/2012 22:33'!
testSimpleEditor
	"
	TextEditorTest new testSimpleEditor
	"
	| m |
	self shouldnt: [
		m _ OneLineEditorMorph new.
		m editor offerMenuFromEsc:
			(KeyboardEvent new
				setType: #keystroke
				buttons: 0
				position: 0@0
				keyValue: 65
				hand: self currentWorld activeHand
				stamp: 0)
	] raise: Exception! !


!TextModelMorph methodsFor: 'focus handling' stamp: 'jmv 9/25/2012 22:27'!
focusText

	self world activeHand newKeyboardFocus: self textMorph! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 9/25/2012 22:52'!
fullScreenOff

	Display fullScreenMode: false.
	DisplayScreen checkForNewScreenSize.
	myWorld restoreMorphicDisplay! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 9/25/2012 22:52'!
fullScreenOn

	Display fullScreenMode: true.
	DisplayScreen checkForNewScreenSize.
	myWorld restoreMorphicDisplay! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 9/25/2012 22:52'!
startMessageTally
	"Tally on all the processes in the system, and not only the UI"
	
	(self confirm: 'MessageTally all the processes in
the system, until the mouse pointer
goes to the top of the screen') ifTrue: [
		MessageTally spyAllOn: [
			[Sensor peekMousePt y > 0] whileTrue: [myWorld doOneCycle]]]! !

!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 9/25/2012 22:52'!
startThenBrowseMessageTally
	"Tally only the UI process"
	
	(self confirm: 'MessageTally the UI process until the
mouse pointer goes to the top of the screen')
		ifTrue: [TimeProfileBrowser
				onBlock: [[Sensor peekMousePt y > 10]
						whileTrue: [myWorld doOneCycle]]]! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 9/25/2012 22:52'!
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
				{ 'Restore Display (r)'. { myWorld. #restoreMorphicDisplay}.
					'repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'}.
				nil.
				{ 'Save'. { Smalltalk . #saveSession}.
					'save the current version of the image on disk'}.
				{ 'Save as...'. { Smalltalk . #saveAs}.
					'save the current version of the image on disk under a new name.'}.
				{ 'Save as New Version'. { Smalltalk . #saveAsNewVersion}.
					'give the current image a new version-stamped name and save it under that name on disk.'}.
				{ 'Save and Quit'. { self. #saveAndQuit}.
					'save the current image on disk, and quit out of Cuis.'}.
				{ 'Quit'. { self. #quitSession}.
					'quit out of Cuis.'}}.
	^menu! !


!Theme class methodsFor: 'class initialization' stamp: 'jmv 9/25/2012 22:52'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 initialize.
	self currentWorld backgroundImage ifNil: [ self currentWorld color: CurrentTheme background ].
	SystemWindow initialize.
	FormCanvas clearFormsCache.
	Taskbar reset; initialize.
	self currentWorld restoreMorphicDisplay.
	
	^ CurrentTheme! !


!Utilities class methodsFor: 'common requests' stamp: 'jmv 9/25/2012 22:53'!
saveScreenshot
	"Make a screenshot of the world and save it to a file"

	"SampledSound playSoundNamed: 'camera'."
	PNGReadWriter
		putForm: (self currentWorld imageForm: 32)
		onFileNamed:
			(FileDirectory default
				nextNameFor: 'CuisScreen'
				extension: 'png')! !

!Utilities class methodsFor: 'closure support' stamp: 'jmv 9/25/2012 22:52'!
postRecompileCleanup	"Utilities postRecompileCleanup"
	"Cleanup after loading closure bootstrap"
	"Before doing this, please start a new UI process (for example, by hitting alt-period and closing the debugger)."
	| unboundMethods contexts |
	self currentWorld removeAllKnownFailing.
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


!WorldState methodsFor: 'update cycle' stamp: 'jmv 9/25/2012 22:40'!
doOneCycleNowFor: aWorld
	"Immediately do one cycle of the interaction loop.
	This should not be called directly, but only via doOneCycleFor:"

	| hadAnyEvent |
	DisplayScreen checkForNewScreenSize.

	"process user input events"
	self handsDo: [ :h |
		activeHand _ h.
		hadAnyEvent _ h processEventQueue.
		activeHand _ nil.
	].

	"the default is the primary hand"
	activeHand _ self hands first.

	aWorld runStepMethods.		"there are currently some variations here"
	self displayWorldSafely: aWorld.

	^hadAnyEvent! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 9/25/2012 22:41'!
doOneSubCycleFor: aWorld
	"Like doOneCycle, but preserves activeHand."

	| currentHand |
	currentHand _ activeHand.
	self doOneCycleFor: aWorld.
	activeHand _ currentHand! !


!WorldState class methodsFor: 'class initialization' stamp: 'jmv 9/25/2012 22:57'!
addDeferredUIMessage: valuableObject

	self deferredUIMessages nextPut: valuableObject! !

!WorldState class methodsFor: 'class initialization' stamp: 'jmv 9/25/2012 22:57'!
deferredUIMessages

	self flag: #jmvVer2.	"make this an inst variable of the world..."
	^DeferredUIMessages ifNil: [DeferredUIMessages _ SharedQueue new].
! !


!WorldTest methodsFor: 'as yet unclassified' stamp: 'jmv 9/25/2012 22:54'!
testDoOneCycleWorksWithDeferredQueue
        "Ensure that nested doOneCycles don't break deferred UI messages"
        | finished |
        [
                WorldState addDeferredUIMessage: [ self currentWorld doOneCycleNow ].
                WorldState addDeferredUIMessage: nil "whatever".
                self currentWorld doOneCycleNow.
                finished _ true.
        ] valueWithin: 1 seconds onTimeout: [finished _ false ].
        self assert: finished! !

!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent activeHand'
	classVariableNames: 'DeferredUIMessages MaxCycleLapse'
	poolDictionaries: ''
	category: 'Morphic-Worlds'!
!methodRemoval: MenuMorph #invokeModalAt:allowKeyboard:!
MenuMorph removeSelector: #invokeModalAt:allowKeyboard:!
!methodRemoval: MenuMorph #positionAt:relativeTo:!
MenuMorph removeSelector: #positionAt:relativeTo:!
!methodRemoval: Morph #activeHand!
Morph removeSelector: #activeHand!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Smalltalk removeKey: #ActiveHand!


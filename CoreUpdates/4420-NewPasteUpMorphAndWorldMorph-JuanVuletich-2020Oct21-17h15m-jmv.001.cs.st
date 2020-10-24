'From Cuis 5.0 [latest update: #4416] on 21 October 2020 at 5:18:31 pm'!
!classDefinition: #PasteUpMorph category: #'Morphic-Kernel'!
KernelMorph subclass: #PasteUpMorph
	instanceVariableNames: 'backgroundImage backgroundImageData'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!PasteUpMorph commentStamp: 'jmv 2/21/2016 18:32' prior: 0!
A World, the entire Smalltalk screen, is a PasteUpMorph.  A World responds true to isWorld.  A PasteUpMorph that is a world, builds its menu in HandMorph buildWorldMenu.

worldState		If I am also a World, keeps the hands, damageRecorder, stepList etc.
!

!classDefinition: #WorldMorph category: #'Morphic-Kernel'!
PasteUpMorph subclass: #WorldMorph
	instanceVariableNames: 'activeHand hands canvas damageRecorder stepList lastCycleTime alarms lastAlarmTime deferredUIMessages drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent taskbar'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!WorldMorph commentStamp: '<historical>' prior: 0!
A WorldMorph is a kind of PasteUpMorph that can act as the root morph of a Morphic system. Usually uses the whole Display.

activeHand 					-- a HandMorph
hands 					-- Array of HandMorphs (typically only one) representing the Cursor and its event queue.
canvas 					-- BitBlitCanvas on the DisplayScreen. (Display getCanvas)
damageRecorder 					-- the DamageRecorder for the Display
stepList 					-- a Heap of StepMessage.  Each morph which wants stepping has a StepMessage here.
								See comment in #Morph>>stepAt:
lastStepTime 					-- 'now' as last sampled at step. (Time localMillisecondClock)
lastStepMessage 					-- nil or a selector
lastCycleTime 					-- 'now' as last sampled (Time localMillisecondClock)
alarms 					-- a Heap of MorphicAlarm.  See comment in #Morph>>addAlarm:after:
								An _alarm_ is an action to be done once, vs a _step_ which is cycled.
lastAlarm 					-- 'now' as sampled at last alarm (Time localMillisecondClock).
drawingFailureMorphs 					-- a WeakIdentitySet of Morphs with drawing failures
waitDelay 					-- a Delay (set in #WorldState>>doOneCycleFor:)
pause 					-- A 'phase-locked loop' style value to help regularise the step/alarm/event service rate.
		 						(set in #WorldState>>doOneCycleFor:)
lastCycleHadAnyEvent 					-- a boolean (set in #WorldState>>doOneCycleFor:)
world 					-- a PasteUpMorph 
!


!PasteUpMorph methodsFor: 'accessing' stamp: 'jmv 10/21/2020 16:24:51'!
activeHand
	"Answer the currently active hand, if any..."
	^self world ifNotNil: [ :w | w activeHand ]! !

!PasteUpMorph methodsFor: 'accessing' stamp: 'jmv 11/19/2010 13:56'!
color: aColor
	super color: aColor.
	self backgroundImageData: nil! !

!PasteUpMorph methodsFor: 'accessing' stamp: 'GC 3/17/2019 08:16:08'!
handlesKeyboard

	^ true ! !

!PasteUpMorph methodsFor: 'caching' stamp: 'jmv 10/21/2020 16:44:28'!
releaseCachedState
	super releaseCachedState.
	backgroundImage _ nil.! !

!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 1/16/2017 09:56:14'!
addedMorph: aMorph
	"Notify the receiver that the given morph was just added."
	super addedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasOpened: aMorph ]! !

!PasteUpMorph methodsFor: 'change reporting' stamp: 'jmv 1/16/2017 09:56:07'!
removedMorph: aMorph
	"Notify the receiver that aMorph was just removed from its children"
	super removedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasDeleted: aMorph ]! !

!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 10/21/2020 16:40:25'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			aCanvas image: backgroundImage at: `0@0` ]
		ifNil: [
			super drawOn: aCanvas ]! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'HAW 12/24/2018 07:13:56'!
allowsFilesDrop

	^ true! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/19/2013 15:24'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^ true! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 1/19/2013 15:25'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false.
	Both 'aMorph allowsGrabWithHand' and 'aMorph owner allowsSubmorphDrag' must be true for aMorph to be grabbed by the hand. It is also required that 'aMorph handlesMouseDown:' be false."

	^ true! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'HAW 12/24/2018 10:27:39'!
dropFiles: aDropFilesEvent

	(DropFilesAction for: aDropFilesEvent) value! !

!PasteUpMorph methodsFor: 'events' stamp: 'jmv 10/28/2019 18:13:11'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."
	(aKeyboardEvent commandAltKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [
			aKeyboardEvent keyCharacter = $b ifTrue: [ BrowserWindow openBrowser ].
			aKeyboardEvent keyCharacter = $f ifTrue: [ BrowserWindow findClass ].
			aKeyboardEvent keyCharacter = $F ifTrue: [ MessageSetWindow findInSourceCode ].
		].
	"aKeyboardEvent keyCharacter print."
	^ super keyStroke: aKeyboardEvent! !

!PasteUpMorph methodsFor: 'events' stamp: 'jmv 1/18/2013 12:38'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PasteUpMorph methodsFor: 'events' stamp: 'jmv 10/21/2020 15:41:41'!
windowEvent: aMorphicEvent

	aMorphicEvent windowEventType == #windowClose
		ifTrue: [
			^TheWorldMenu basicNew quitSession]
! !

!PasteUpMorph methodsFor: 'event handling testing' stamp: 'jmv 8/20/2012 18:56'!
handlesMouseDown: aMouseButtonEvent
	^true! !

!PasteUpMorph methodsFor: 'event handling' stamp: 'jmv 3/10/2011 16:02'!
mouseButton2Activity

	^self invokeWorldMenu! !

!PasteUpMorph methodsFor: 'event handling' stamp: 'jmv 10/21/2020 16:48:08'!
wantsWindowEvent: anEvent
	^false! !

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 5/24/2020 10:07:38'!
fontPreferenceChanged
	self submorphsDo: [ :m |
		m morphExtent: (m morphExtent max: m minimumExtent).
		m fontPreferenceChanged ]! !

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 16:33:43'!
privateExtent: newExtent

	^ (super privateExtent: newExtent)
		ifTrue: [ self buildMagnifiedBackgroundImage ];
		yourself! !

!PasteUpMorph methodsFor: 'initialization' stamp: 'jmv 3/10/2018 20:58:35'!
defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ `Color
		r: 0.861
		g: 1.0
		b: 0.722`! !

!PasteUpMorph methodsFor: 'initialization' stamp: 'dgd 2/14/2003 22:38'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 1! !

!PasteUpMorph methodsFor: 'initialization' stamp: 'jmv 3/10/2018 20:58:44'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ `Color
		r: 0.8
		g: 1.0
		b: 0.6`! !

!PasteUpMorph methodsFor: 'interaction loop' stamp: 'jmv 8/17/2020 13:41:37'!
mainLoop

	
	self clearWaitDelay.
	self setCanvas.
	[
		self doOneCycle.
		Processor yield.
		true ]
			whileTrue: []! !

!PasteUpMorph methodsFor: 'interaction loop' stamp: 'HAW 12/26/2019 10:05:45'!
runProcess
	
	| process |
	
	process _ [ self mainLoop ] newProcess.
	process
		priority: Processor userSchedulingPriority;
		name: 'Morphic UI';
		animatedUI: self.
	
	^ process! !

!PasteUpMorph methodsFor: 'menu & halo' stamp: 'jmv 1/19/2013 15:14'!
addWorldHaloMenuItemsTo: aMenu hand: aHandMorph
	"Add standard halo items to the menu, given that the receiver is a World"

	| unlockables |
	self addColorMenuItems: aMenu hand: aHandMorph.

"	aMenu addLine.
	self addWorldToggleItemsToHaloMenu: aMenu."
	aMenu addLine.
	self addCopyItemsTo: aMenu.
	self addExportMenuItems: aMenu hand: aHandMorph.

	self addDebuggingItemsTo: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu defaultTarget: self.

	aMenu addLine.

	unlockables _ self submorphs select:
		[ :m | m isLocked].
	unlockables size = 1 ifTrue: [
		aMenu add: ('unlock "{1}"' format:{unlockables first printStringLimitedTo: 40})action: #unlockContents].
	unlockables size > 1 ifTrue: [
		aMenu add: 'unlock all contents' action: #unlockContents.
		aMenu add: 'unlock...' action: #unlockOneSubpart].

	aMenu defaultTarget: aHandMorph.
! !

!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 3/2/2011 11:10'!
backgroundImage
	^backgroundImage! !

!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 12/8/2013 15:11'!
backgroundImageData: aByteArray
	"
	| filename |
	filename _ 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	filename _ 'bg/free-desktop-wallpaper.jpg'.
	filename _ 'bg/jellyfish-thumb.jpg'.
	filename _ 'bg/splash_by_beefpepsi.jpg'.
	filename _ 'bg/gray ocean and pier.jpg'.
	filename _ 'bg/newyork.jpg'.
	filename _ 'bg/download-free-desktop-wallpaper-nature-conquestofparadise-marirs-pic.jpg'.
	filename _ 'bg/desktop-wallpaper-tropical-1280x1024.jpg'.

	filename _ 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	self runningWorld backgroundImageData: (FileStream readOnlyFileNamed: filename) binary contentsOfEntireFile.
	"
	backgroundImageData _ aByteArray.
	self buildMagnifiedBackgroundImage! !

!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 10/21/2020 16:30:09'!
buildMagnifiedBackgroundImage
	| image old |
	old _ backgroundImage.
	backgroundImageData
		ifNil: [ backgroundImage _ nil ]
		ifNotNil: [ 
			[image _ Form fromBinaryStream: backgroundImageData readStream.
			backgroundImage _ image magnifyTo: extent.
			] on: Error do: [backgroundImage := nil]. "Can happen if JPEG plugin not built"
		].
	old == backgroundImage ifFalse: [
		self redrawNeeded ]! !

!PasteUpMorph methodsFor: 'stepping and presenter' stamp: 'jmv 6/11/2012 09:59'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!!) don't need to answer true to this message.
	jmv: Not really sure. Sub-world stepping needs some review."

	^true! !

!PasteUpMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/23/2020 14:47:46'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect _ Rectangle center: aPoint extent: aMorph morphExtent.
	delta _ trialRect amountToTranslateWithin: self displayBounds.
	self addMorph: aMorph.
	aMorph morphPositionInWorld: trialRect origin + delta.! !

!PasteUpMorph methodsFor: 'testing' stamp: 'jmv 3/9/2020 10:13:47'!
is: aSymbol
	^ aSymbol == #PasteUpMorph or: [ super is: aSymbol ]! !

!PasteUpMorph methodsFor: 'testing' stamp: 'jmv 10/21/2020 16:42:59'!
isReallyVisible
	"Answer true only if all the owner chain is visible (i.e. if we are really visible!!)"
	^self visible and: [owner isReallyVisible ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 09:17'!
bringWindowsFullOnscreen
	"Make ever SystemWindow on the desktop be totally on-screen, whenever possible."
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ]) do: [ :each |
		each makeMeFullyVisible ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 09:18'!
closeUnchangedWindows
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."
	(SelectionMenu confirm: 'Do you really want to close all windows
except those with unaccepted edits?') ifFalse: [ ^ self ].
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 1/24/2016 21:58'!
collapseNonWindows
	self allNonWindowRelatedSubmorphs do: [ :m |
		m collapse]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 1/24/2016 21:58'!
deleteNonWindows
	(SelectionMenu confirm:
'Do you really want to discard all objects
that are not in windows?')
		ifFalse: [^ self].

	self allNonWindowRelatedSubmorphs do: [:m |
		m delete ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 3/21/2012 17:23'!
findAChangeSorter: evt
	"Locate a change sorter, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model isMemberOf: ChangeSorter]
		orMakeOneUsing: [ ChangeSorterWindow open: ChangeSorter new label: nil ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 7/4/2016 22:20'!
findAFileList: evt
	"Locate a file list, open it, and bring it to the front.
	Create one if necessary, respecting the Preference."
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == FileList ]
		orMakeOneUsing: [
			FileListWindow openFileList ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 7/4/2016 22:21'!
findAMessageNamesWindow: evt
	"Locate a MessageNames tool, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == MessageNames ]
		orMakeOneUsing: [
			MessageNamesWindow open: MessageNames new label: 'Message Names' ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 7/30/2014 09:01'!
findATranscript: evt
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self findATranscript! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 10/25/2010 21:29'!
findAWindowSatisfying: qualifyingBlock orMakeOneUsing: makeBlock
	"Locate a window satisfying a block, open it, and bring it to the front.  Create one if necessary, by using the makeBlock"
	| aWindow |
	submorphs do: [ :aMorph |
		(((aWindow _ aMorph) is: #SystemWindow) and: [ qualifyingBlock value: aWindow ]) ifTrue: [
			aWindow isCollapsed ifTrue: [ aWindow expand ].
			aWindow activateAndForceLabelToShow.
			^ self ]].
	"None found, so create one"
	makeBlock value.! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 7/5/2016 09:42'!
findDirtyBrowsers: evt
	"Present a menu of window titles for browsers with changes,
	and activate the one that gets chosen."
	| menu |
	menu _ MenuMorph new.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [
				(w model is: #CodeProvider) and: [ w canDiscardEdits not ]]]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activate ].
	menu submorphs notEmpty ifTrue: [ menu popUpInWorld: self ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 8/13/2013 09:20'!
findDirtyWindows: evt
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."
	| menu |
	menu _ MenuMorph new.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits not ]]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activate ].
	menu submorphs notEmpty ifTrue: [ menu popUpInWorld: self ]! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 3/10/2018 22:02:53'!
findWindow: evt
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."
	| menu expanded collapsed nakedMorphs |
	menu _ MenuMorph new.
	expanded _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ].
	collapsed _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed ].
	nakedMorphs _ self submorphsSatisfying: [ :m |
		(m is: #SystemWindow) not ].
	expanded isEmpty & (collapsed isEmpty & nakedMorphs isEmpty) ifTrue: [ ^ Smalltalk beep ].
	(expanded asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activateAndForceLabelToShow.
		w canDiscardEdits ifFalse: [ menu lastItem color: `Color red` ]].
	expanded isEmpty | (collapsed isEmpty & nakedMorphs isEmpty) ifFalse: [ menu addLine ].
	(collapsed asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #expand.
		w canDiscardEdits ifFalse: [ menu lastItem color: `Color red` ]].
	nakedMorphs isEmpty ifFalse: [ menu addLine ].
	(nakedMorphs asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #comeToFrontAndAddHalo ].
	menu addTitle: 'find window'.
	menu popUpInWorld: self! !

!PasteUpMorph methodsFor: 'world menu' stamp: 'HAW 7/5/2018 18:24:37'!
invokeWorldMenu
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed"

	| menu |
	menu _ (TheWorldMenu new 
		world: self
		hand: self activeHand) buildWorldMenu.
	menu addTitle: Preferences desktopMenuTitle.
	menu popUpInWorld: self! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 1/8/2017 16:44:57'!
allNonWindowRelatedSubmorphs
	"Answer all non-window submorphs that are not flap-related"

	^submorphs 
		reject: [ :m | (m is: #SystemWindow) or: [ m is: #TaskbarMorph ] ]! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 2/28/2011 15:16'!
deleteAllHalos
	self haloMorphs do:
		[ :m | m delete]! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 9/13/2013 09:18'!
fullRepaintNeeded
	self redrawNeeded.
	SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ifTrue: [ w makeMeVisible ].
			false ]! !

!PasteUpMorph methodsFor: 'world state' stamp: 'ar 9/28/2000 18:00'!
haloMorphs
	^ self hands collect:[:h| h halo] thenSelect:[:halo| halo notNil]! !

!PasteUpMorph methodsFor: 'halos and balloon help' stamp: 'jmv 10/21/2020 16:46:33'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"
	(#(addHelpHandle: addRotateHandle: addRecolorHandle:) statePointsTo: aSelector)
		ifTrue: ["FIXME - hack to disable for non-functional halo items"
			^ false].
	
	^super wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph! !

!PasteUpMorph methodsFor: 'object serialization' stamp: 'jmv 6/8/2014 18:28'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	| dp |
	dp _ DiskProxy global: #Smalltalk selector: #runningWorld args: #().
	refStrm replace: self with: dp.
	^ dp! !

!PasteUpMorph methodsFor: 'windows' stamp: 'jmv 7/30/2014 09:00'!
findATranscript
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self
		findAWindowSatisfying: [ :aWindow | aWindow model == Transcript]
		orMakeOneUsing: [ TranscriptWindow openTranscript ]! !

!PasteUpMorph methodsFor: 'ui services' stamp: 'jmv 5/23/2020 21:00:01'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	"This is deprecated because using it requires blocking the user interface until the user provides a response. Please use the variants that are not modal."
	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock! !

!PasteUpMorph methodsFor: 'ui services' stamp: 'jmv 5/23/2020 21:00:05'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock! !


!PasteUpMorph class methodsFor: 'system startup' stamp: 'jmv 10/21/2020 16:49:47'!
initClassCachedState
	"Should use some other way to find relevant instances"
	self flag: #jmvVer2.
	self allInstancesDo: [ :each |
		each buildMagnifiedBackgroundImage ].
	Cursor defaultCursor activateCursor.! !

!PasteUpMorph class methodsFor: 'new-morph participation' stamp: 'pb 6/8/2017 23:51:39'!
categoryInNewMorphMenu
	^ 'Worlds'! !


!WorldMorph methodsFor: 'alarms' stamp: 'jmv 3/3/2017 09:33:59'!
addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime
	"Add a new alarm with the given set of parameters"

	alarms add: 
		(MorphicAlarm 
			receiver: aTarget
			selector: aSelector
			arguments: argArray
			at: scheduledTime)! !

!WorldMorph methodsFor: 'alarms' stamp: 'jmv 3/3/2017 09:34:03'!
adjustAlarmTimes: nowTime
	"Adjust the alarm times after some clock weirdness (such as image-startup etc)"
	| deltaTime |
	deltaTime _ nowTime - lastAlarmTime.
	alarms do: [ :alarm |
		alarm scheduledTime: alarm scheduledTime + deltaTime ]! !

!WorldMorph methodsFor: 'alarms' stamp: 'jmv 5/11/2016 09:53'!
alarmSortBlock

	^[ :alarm1 :alarm2 | alarm1 scheduledTime < alarm2 scheduledTime ]! !

!WorldMorph methodsFor: 'alarms' stamp: 'jmv 3/3/2017 11:54:00'!
removeAlarm: aSelector for: aTarget 
	"Remove the alarm with the given selector"

	| alarm |
	alarm _ alarms 
				detect: [ :any | any receiver == aTarget and: [any selector == aSelector]]
				ifNone: nil.
	alarm ifNotNil: [ alarms remove: alarm ]! !

!WorldMorph methodsFor: 'alarms' stamp: 'jmv 3/3/2017 09:35:03'!
triggerAlarmsBefore: nowTime
	"Trigger all pending alarms that are to be executed before nowTime."

	nowTime - lastAlarmTime > 10000
		ifTrue: [ self adjustAlarmTimes: nowTime ].
	[ alarms notEmpty and: [ alarms first scheduledTime < nowTime ]]
		whileTrue: [ alarms removeFirst valueAtTime: nowTime ].
	lastAlarmTime _ nowTime! !

!WorldMorph methodsFor: 'canvas' stamp: 'di 6/7/1999 17:44'!
canvas

	^ canvas! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 8/17/2020 13:42:46'!
clearCanvas
	canvas _ nil.
	damageRecorder _ nil.! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 10/21/2020 15:17:06'!
ensureNonDisplayCanvas
	(canvas isNil or: [
		canvas drawsOnDisplay or: [
		(canvas extent ~= self morphExtent) or: [
		canvas form depth ~= Display depth]]]) ifTrue: [
			"allocate a new offscreen canvas the size of the window"
			self setCanvas: (MorphicCanvas withExtent: self morphExtent depth: Display depth)]! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 8/4/2020 10:02:25'!
recordDamagedRect: damageRect for: aMorph

	damageRecorder ifNotNil: [
		damageRecorder recordInvalidRect: damageRect for: aMorph ]! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 10/21/2020 15:17:17'!
setCanvas
	self setCanvas: Display getCanvas.
	damageRecorder _ DamageRecorder new.
	self redrawNeeded! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 10/21/2020 15:17:24'!
setCanvas: aMorphicCanvas
	canvas _ aMorphicCanvas.
	canvas world: self.
	damageRecorder
		ifNil: [ damageRecorder _ DamageRecorder new].! !

!WorldMorph methodsFor: 'hands' stamp: 'jmv 9/25/2012 22:39'!
activeHand
	^activeHand! !

!WorldMorph methodsFor: 'hands' stamp: 'di 6/7/1999 17:40'!
hands

	^ hands! !

!WorldMorph methodsFor: 'hands' stamp: 'RAA 5/24/2000 10:13'!
handsDo: aBlock

	^ hands do: aBlock! !

!WorldMorph methodsFor: 'hands' stamp: 'RAA 5/24/2000 12:09'!
handsReverseDo: aBlock

	^ hands reverseDo: aBlock! !

!WorldMorph methodsFor: 'hands' stamp: 'jmv 7/22/2020 20:42:49'!
selectHandsToDrawForDamage: damageList
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	| result |
	result _ OrderedCollection new.
	hands do: [:hand |
		hand needsToBeDrawn ifTrue: [
			hand hasChanged
				ifTrue: [result add: hand]
				ifFalse: [
					hand displayFullBounds ifNotNil: [ :handBounds |
						(damageList anySatisfy: [ :r | r intersects: handBounds]) ifTrue: [
							result add: hand]]]]].
	^ result! !

!WorldMorph methodsFor: 'initialization' stamp: 'jmv 6/20/2014 20:24:55'!
clearWaitDelay
	waitDelay ifNotNil: [
		waitDelay unschedule.
		waitDelay _ nil ].
	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime _ Time localMillisecondClock.
	lastAlarmTime _ 0.! !

!WorldMorph methodsFor: 'initialization' stamp: 'jmv 10/21/2020 16:05:30'!
initialize

	super initialize.
	activeHand _ HandMorph new.
	hands _ { activeHand }.
	damageRecorder _ DamageRecorder new.
	stepList _ Heap sortBlock: self stepListSortBlock.
	alarms _ Heap sortBlock: self alarmSortBlock.
	lastAlarmTime _ 0.
	deferredUIMessages _ SharedQueue new.
	drawingFailingMorphs _ WeakIdentitySet new.
	pause _ 20.
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ false! !

!WorldMorph methodsFor: 'initialization' stamp: 'jmv 2/2/2014 21:05'!
stepListSortBlock

	^ [ :stepMsg1 :stepMsg2 |
		stepMsg1 scheduledTime <= stepMsg2 scheduledTime ]! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 10/21/2020 15:16:15'!
cleanseStepList
	"Remove morphs from the step list that are not in this World."

	| deletions |
	deletions _ OrderedCollection new.
	stepList do: [ :entry |
		entry receiver world == self ifFalse: [
			deletions add: entry]].
	deletions do: [ :entry|
		stepList remove: entry ].

	deletions _ OrderedCollection new.
	alarms do: [ :entry |
		((entry receiver is: #Morph) and: [ entry receiver world == self ]) ifFalse: [
			deletions add: entry]].
	deletions do: [ :entry|
		alarms remove: entry ]! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 10/21/2020 15:35:50'!
runLocalStepMethods: nowTime
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world."

	| stepMessage |
	[ stepList notEmpty and: [ stepList first scheduledTime <= nowTime ]] 
		whileTrue: [
			stepMessage _ stepList first.
			(stepMessage receiver shouldGetStepsFrom: self)
				ifFalse: [ stepList removeFirst ]
				ifTrue: [
					stepMessage valueAtTime: nowTime.
					stepMessage rescheduleAfter: nowTime.
					"We've just updated the  scheduled time for stepMessage.
					It might have been that stepMessage was removed altogether from stepList.
					It also may be the case that stepList got added or removed other elements while on #valueAtTime:
					Just reSort. It will be ok in any case."
					stepList reSort.
					]
		]! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 10/21/2020 15:34:17'!
runStepMethods
	"Perform periodic activity inbetween event cycles"
	| readyToProcess |

	"Processing the queue until empty is wrong if a block in it calls #addDeferredUIMessage: itself, because this loop will never end.
	Instead, process no more than entries already in queue befor we start iterating!!"
	readyToProcess _ deferredUIMessages size.
	readyToProcess timesRepeat: [
		deferredUIMessages nextOrNil ifNotNil: [ :block |
			block value
		]
	].
	self triggerAlarmsBefore: lastCycleTime.
	self runLocalStepMethods: lastCycleTime.

	"we are using a normal #step for these now"
	"aWorld allLowerWorldsDo: [ :each | each runLocalStepMethods ]."
! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 2/3/2014 21:15'!
startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTimeOrNil
	"Add the given morph to the step list"

	self stopStepping: aMorph selector: aSelector.
	stepList add: (
		StepMessage 
			receiver: aMorph
			selector: aSelector
			at: scheduledTime
			stepTime: stepTimeOrNil)! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 3/3/2017 11:39:34'!
stopStepping: aMorph selector: aSelector
	"Remove the given morph from the step list."
	stepList removeAll: (stepList select:[:stepMsg| stepMsg receiver == aMorph and: [ stepMsg selector == aSelector ]])! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 3/3/2017 11:39:22'!
stopSteppingMorph: aMorph
	"Remove the given morph from the step list."
	stepList removeAll: (stepList select: [ :stepMsg | stepMsg receiver == aMorph])! !

!WorldMorph methodsFor: 'update cycle' stamp: 'RAA 5/24/2000 13:13'!
checkIfUpdateNeeded

	damageRecorder updateIsNeeded ifTrue: [^true].
	hands do: [:h | (h hasChanged and: [h needsToBeDrawn]) ifTrue: [^true]].
	^false  "display is already up-to-date"
! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/21/2020 15:16:56'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[self displayWorld] ifError: [:err :rcvr |
		"Handle a drawing error"
		| errCtx errMorph |
		errCtx _ thisContext.
		[
			errCtx _ errCtx sender.
			"Search the sender chain to find the morph causing the problem"
			[errCtx notNil and: [ (errCtx receiver is: #Morph) not ]] 
				whileTrue:[errCtx _ errCtx sender].
			"If we're at the root of the context chain then we have a fatal drawing problem"
			errCtx ifNil:[^self handleFatalDrawingError: err].
			errMorph _ errCtx receiver.
			"If the morph causing the problem has already the #drawError flag set,
			then search for the next morph above in the caller chain."
			errMorph isKnownFailing
		] whileTrue.
		errMorph drawingFails.
		self setCanvas.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	]! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/21/2020 15:33:56'!
doOneCycle
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	lastCycleHadAnyEvent | deferredUIMessages isEmpty not
		ifTrue: [
			pause _ 20.				"This value will only be used when there are no more events to serve."
			wait _ 0 ]					"Don't wait"
		ifFalse: [
			pause < 200				"No events processed? Start saving CPU!! But never make the user wait more than 200ms for being responsive again."
				ifTrue: [ pause _ pause * 21//20 ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].

			wait _ waitUntil - Time localMillisecondClock ].
	Preferences serverMode
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait > 0
		ifFalse: [ Processor yield ]
		ifTrue: [
			waitDelay beingWaitedOn
				ifFalse: [ waitDelay setDelay: wait; wait ]
				ifTrue: [
					"If we are called from a different process than that of the main UI, we might be called in the main
					interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
					(Delay forMilliseconds: wait) wait ]].

	"Record start time of this cycle, and do cycle"
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNow! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/21/2020 16:04:46'!
doOneCycleNow
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| hadAnyEvent |
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	DisplayScreen checkForNewScreenSize.
	self displayWorldSafely.
	"Run steps, alarms and deferred UI messages"
	self runStepMethods.
	"Process user input events. Run all event triggered code."
	hadAnyEvent _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAnyEvent _ hadAnyEvent | h processEventQueue.
		activeHand _ nil ].
	"The default is the primary hand"
	activeHand _ self hands first.
	^ hadAnyEvent.! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 3/2/2017 20:06:48'!
doOneMinimalCycleNow
	"Immediately do one cycle of the interaction loop.
	Only repair display and process events. For modal menus and such."

	"Repair visual damage."
	self displayWorldSafely.

	"Process user input events. Run all event triggered code."
	^activeHand processEventQueue! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 6/26/2015 14:36'!
handleFatalDrawingError: errMsg
	"Handle a fatal drawing error."

	self primitiveError: 
		'Fatal Morphic drawing error', String newLineString,
		errMsg.

	"Hm... we should jump into a 'safe' WorldMorph here, but how do we find it?!!"! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/21/2020 15:17:28'!
tryDeferredUpdatingAndSetCanvas
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, set a non-Disply canvas and answer false.."
	| properDisplay |
	
	"As this is the only sender of #deferUpdates: , this could be done in Morphic or image startup, and never efterwards..."
	(Display deferUpdates: true) ifNil: [
		"deferred updates not supported by the VM, do them in the image!!"
		self ensureNonDisplayCanvas.
		^ false].
	
	"if no canvas, or canvas was offscreen, from a platform that didnt support defers, then fix it"
	properDisplay _ canvas notNil and: [canvas drawsOnDisplay].
	properDisplay ifFalse: [
		self morphPosition: `0@0` extent: Display extent.
		self setCanvas: Display getCanvas.
	].
	^ true! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/21/2020 15:34:22'!
whenUIinSafeState: evaluableObject
	"Please call
	UISupervisor whenUIinSafeState: evaluableObject
	"
	deferredUIMessages nextPut: evaluableObject! !

!WorldMorph methodsFor: 'errors on draw' stamp: 'jmv 11/5/2007 22:59'!
addKnownFailing: aMorph
	drawingFailingMorphs add: aMorph! !

!WorldMorph methodsFor: 'errors on draw' stamp: 'jmv 11/5/2007 22:59'!
isKnownFailing: aMorph
	^drawingFailingMorphs includes: aMorph! !

!WorldMorph methodsFor: 'errors on draw' stamp: 'jmv 10/21/2020 15:54:35'!
removeAllKnownFailing
	drawingFailingMorphs _ WeakIdentitySet new.
	self redrawNeeded! !

!WorldMorph methodsFor: 'errors on draw' stamp: 'jmv 11/5/2007 22:59'!
removeKnownFailing: aMorph
	drawingFailingMorphs remove: aMorph! !

!WorldMorph methodsFor: 'drawing' stamp: 'jmv 10/21/2020 17:06:37'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"repair world's damage on canvas"
	worldDamageRects _ canvas drawWorld: self repair: damageRecorder.

	"Check which hands need to be drawn (they are not the hardware mouse pointer)"
	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"Draw hands (usually carying morphs) onto world canvas"
	canvas newClipRect: nil.
	handsToDraw reverseDo: [ :h | canvas fullDrawHand: h ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		"Drawing was done to off-Display canvas. Copy content to Display"
		canvas showAt: self viewBox origin invalidRects: allDamage ].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	Display forceDamageToScreen: allDamage.

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].! !

!WorldMorph methodsFor: 'drawing' stamp: 'jmv 10/21/2020 16:41:02'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			aCanvas image: backgroundImage at: `0@0` ]
		ifNil: [
			"draw background fill"
			(aCanvas drawsOnDisplay and: [ color mightBeTranslucent ])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Cuis Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: `0@0` in: nil
						fillColor: color rule: Form over.
					Display forceToScreen]
				ifFalse: [ super drawOn: aCanvas ]]! !

!WorldMorph methodsFor: 'event handling' stamp: 'jmv 10/21/2020 16:48:04'!
wantsWindowEvent: anEvent
	^true! !

!WorldMorph methodsFor: 'classification' stamp: 'jmv 10/21/2020 15:47:06'!
isWorldMorph

	^ true! !

!WorldMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 10/21/2020 16:26:04'!
allMorphsDo: aBlock
	"Enumerate all morphs in the world, including those held in hands."

	super allMorphsDo: aBlock.
	self handsReverseDo: [:h | h allMorphsDo: aBlock].! !

!WorldMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 10/21/2020 16:31:21'!
canHandle: aMorph

	^ canvas canDraw: aMorph! !

!WorldMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 10/21/2020 15:49:31'!
firstHand

	^ hands first! !

!WorldMorph methodsFor: 'change reporting' stamp: 'jmv 10/21/2020 15:50:52'!
invalidateDisplayRect: damageRect fromSubmorph: submorphOrNil for: aMorph
        "Clip damage reports to my bounds, since drawing is _always_ clipped to my bounds."

	self recordDamagedRect: (damageRect intersect: self morphLocalBounds ) for: aMorph! !

!WorldMorph methodsFor: 'testing' stamp: 'jmv 10/21/2020 16:43:17'!
isReallyVisible
	"Answer true only if all the owner chain is visible (i.e. if we are really visible!!)"
	^self visible! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 16:39:39'!
displayBounds
	^0@0 extent: extent! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 16:41:28'!
externalizeDisplayBounds: r

	^ r! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 16:41:55'!
externalizeToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates."
	^ aPoint! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 16:42:23'!
internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."
	^ aPoint! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 16:43:37'!
morphPositionInWorld

	self flag: #jmvVer2. "Solo para evitar los warning por falta de owner... pensar despues este caso"
	^ `0@0`! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 15:52:21'!
privateExtent: newExtent

	^ (super privateExtent: newExtent)
		ifTrue: [
			self setCanvas ];
		yourself! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 10/21/2020 15:55:32'!
viewBox

	^ self morphLocalBounds! !

!WorldMorph methodsFor: 'caching' stamp: 'jmv 10/21/2020 15:54:07'!
releaseCachedState
	super releaseCachedState.
	self cleanseStepList.
	self clearCanvas! !

!WorldMorph methodsFor: 'structure' stamp: 'jmv 10/21/2020 15:56:00'!
world
	^self! !

!WorldMorph methodsFor: 'misc' stamp: 'jmv 10/21/2020 16:29:57'!
buildMagnifiedBackgroundImage
	super buildMagnifiedBackgroundImage.

	canvas ifNotNil: [ :c |
		(backgroundImage depth = 32 and: [ c form depth < 32 ]) ifTrue: [
			backgroundImage _ backgroundImage orderedDither32To16 ]]! !

!WorldMorph methodsFor: 'misc' stamp: 'jmv 10/21/2020 16:52:25'!
fillRects: rectangleList
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."


	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: `0@0`;
		clipRect: self viewBox;
		combinationRule: Form over.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt fillColor: Color random.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect ].

	(Delay forMilliseconds: 50) wait! !

!WorldMorph methodsFor: 'misc' stamp: 'jmv 10/21/2020 16:36:53'!
restoreDisplay
	self
		morphExtent: Display extent;
		handsDo: [ :h | h visible: true ];
		fullRepaintNeeded! !

!WorldMorph methodsFor: 'menu & halo' stamp: 'jmv 10/21/2020 16:39:09'!
addCustomMenuItems: menu hand: aHandMorph 
	"Add morph-specific menu itemns to the menu for the hand"

	super addCustomMenuItems: menu hand: aHandMorph.
	menu 
		add: 'desktop menu...'
		target: self
		action: #invokeWorldMenu.
	menu addLine! !

!WorldMorph methodsFor: 'printing' stamp: 'jmv 10/21/2020 16:44:07'!
printOn: aStream
	"Reimplemented to add a tag showing that the receiver is currently functioning as a 'world', if it is"

	aStream nextPutAll: ' [world]'! !

!WorldMorph methodsFor: 'halos and balloon help' stamp: 'jmv 10/21/2020 16:46:19'!
deleteBalloonTarget: aMorph
	"Delete the balloon help targeting the given morph"
	self handsDo:[:h| h deleteBalloonTarget: aMorph].! !

!WorldMorph methodsFor: 'halos and balloon help' stamp: 'jmv 10/21/2020 16:47:14'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	^#(addDebugHandle: addMenuHandle: addHelpHandle:)
		statePointsTo: aSelector! !

!WorldMorph methodsFor: 'taskbar' stamp: 'jmv 10/21/2020 17:00:39'!
hideTaskbar
	taskbar ifNotNil: [
		taskbar delete.
		taskbar _ nil ]! !

!WorldMorph methodsFor: 'taskbar' stamp: 'jmv 10/21/2020 17:00:47'!
showTaskbar

	taskbar ifNil: [
		taskbar _ TaskbarMorph newRow.
		taskbar openInWorld: self ]! !

!WorldMorph methodsFor: 'taskbar' stamp: 'jmv 10/21/2020 17:00:30'!
taskbar
	^taskbar! !

!WorldMorph methodsFor: 'taskbar' stamp: 'jmv 10/21/2020 17:01:03'!
taskbarDeleted
	taskbar _ nil! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/21/2020 17:04:22'!
collapseAll
	"Collapse all windows"
	(SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ])
		reverseDo: [ :w | w collapse.  self displayWorld].
	self collapseNonWindows! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/21/2020 17:00:27'!
restoreAll
	"Expand all windows to their previous state"
	taskbar
		ifNotNil: [ :tb | tb restoreAll ]! !

!WorldMorph methodsFor: 'defaul desktop' stamp: 'jmv 10/21/2020 17:08:07'!
recreateDefaultDesktop
	| editor |
	self whenUIinSafeState: [
		self hideTaskbar.
		(submorphs
			select: [ :ea | ea class == SystemWindow or: [ea class == TranscriptWindow]])
				do: [ :ea | ea delete ].
		TranscriptWindow openTranscript
			morphPosition: 5 @ 283;
			morphExtent: 990 @ 400.
		editor _ TextEditor openTextEditor
			morphPosition: 456 @ 3;
			morphExtent: 900 @ 680.
		editor setLabel: 'About Cuis'.
		editor model actualContents: Utilities defaultTextEditorContents.
		Theme current useTaskbar ifTrue: [self showTaskbar].
	].! !

!WorldMorph methodsFor: 'defaul desktop' stamp: 'jmv 10/21/2020 17:08:22'!
tearDownDesktop
	self whenUIinSafeState: [
		self hideTaskbar.
		submorphs 
			do: [ :ea | (ea is: #SystemWindow) ifTrue: [ ea delete ]]].! !

!WorldMorph methodsFor: 'events' stamp: 'jmv 10/21/2020 17:11:53'!
click: aMouseButtonEvent localPosition: localEventPosition
	^self whenUIinSafeState: [self mouseButton2Activity]! !


!WorldMorph class methodsFor: 'instance creation' stamp: 'jmv 10/21/2020 15:38:59'!
newWorld
	"
[
	UISupervisor stopUIProcess.
	UISupervisor spawnNewMorphicProcessFor: WorldMorph newWorld
] fork.
	"
	| w |
	w _ self new.
	w morphPosition: `0@0` extent: Display extent.
	w setCanvas: Display getCanvas.
	w handsDo: [ :h |
		h privateOwner: w ].
	^w! !

!WorldMorph class methodsFor: 'system startup' stamp: 'jmv 10/21/2020 16:49:42'!
initClassCachedState
	"Should use some other way to find relevant instances"
	self flag: #jmvVer2.
	self allInstancesDo: [ :each |
		each buildMagnifiedBackgroundImage.
		each redrawNeeded ].
	Cursor defaultCursor activateCursor.! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 10/21/2020 17:02:09'!
preferencesMenu
	"Build the preferences menu for the world."

	^ (self menu: 'Preferences...')
		addItemsFromDictionaries: {
			{
				#label 			-> 		'Focus follows mouse'.
				#object 			-> 		Preferences.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#label 			-> 		'Click to focus'.
				#object 			-> 		Preferences.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#label 			-> 		'Font Sizes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#label			->		'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#promptUserAndSetDefault.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current system font family.'
			} asDictionary.
			{
				#label			->		'Load all TrueType Fonts'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#readAdditionalTrueTypeFonts.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Full screen on'.
				#selector 			-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#label 			-> 		'Full screen off'.
				#selector 			-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 			-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#label 			-> 		'All preferences...'.
				#object 			-> 		Preferences.
				#selector 			-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}! !

!TheWorldMenu methodsFor: 'windows & flaps menu' stamp: 'jmv 10/21/2020 16:56:57'!
windowsMenu
        "Build the windows menu for the world."

	^ (self menu: 'Windows')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Find Window'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findWindow:.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Presents a list of all windows; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#label 			-> 		'Find changed Browsers...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyBrowsers:.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Presents a list of browsers that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#label 			-> 		'Find changed Windows...'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findDirtyWindows:.
				#icon 			-> 		#newWindowIcon.
				#balloonText 	-> 		'Presents a list of all windows that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Find a Transcript'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findATranscript:.
				#icon 			-> 		#printerIcon.
				#balloonText 	-> 		'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find a FileList'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAFileList:.
				#icon 			-> 		#systemFileManagerIcon.
				#balloonText 	-> 		'Brings an open fileList  to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find a Change Sorter'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAChangeSorter:.
				#icon 			-> 		#changesIcon.
				#balloonText 	-> 		'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find Message Names'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#findAMessageNamesWindow:.
				#icon 			-> 		#inspectIcon.
				#balloonText 	-> 		'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Tile open windows'.
				#object          ->       					TileResizeMorph.
				#selector 			-> 		#tileOpenWindows.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Tile open windows'.
			} asDictionary.
			{
				#label 			-> 		'Collapse all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#collapseAll.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'Reduce all open windows to collapsed forms that only show titles.'
			} asDictionary.
			{
				#label 			-> 		'Restore all Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#restoreAll.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'Restore all collapsed windows back to their previous forms.'
			} asDictionary.
			{
				#label 			-> 		'Close top Window'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#closeTopWindow.
				#icon 			-> 		#closeIcon.
				#balloonText 	-> 		'Close the topmost window if possible.'
			} asDictionary.
			{
				#label 			-> 		'Send top Window to back'.
				#object 			-> 		SystemWindow.
				#selector 			-> 		#sendTopWindowToBack.
				#icon 			-> 		#goBottomIcon.
				#balloonText 	-> 		'Make the topmost window become the backmost one, and activate the window just beneath it.'
			} asDictionary.
			{
				#label 			-> 		'Move Windows onscreen'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#bringWindowsFullOnscreen.
				#icon 			-> 		#displayIcon.
				#balloonText 	-> 		'Make all windows fully visible on the screen'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Delete unchanged Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#closeUnchangedWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all windows that do not have unsaved text edits.'
			} asDictionary.
			{
				#label 			-> 		'Delete non Windows'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#deleteNonWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all non-window morphs lying on the world.'
			} asDictionary.
			{
				#label 			-> 		'Delete Both of the Above'.
				#selector 			-> 		#cleanUpWorld.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all unchanged windows and also all non-window morphs lying on the world, other than flaps.'
			} asDictionary.
		}`! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
[
	| oldWorld ba roots w |
	oldWorld _ UISupervisor ui.
	ba _ oldWorld instVarNamed: 'backgroundImageData'.
	roots _ oldWorld instVarNamed: 'submorphs'.
	roots _ roots reject: [ :m | m is: #TaskbarMorph ].
	oldWorld _ nil.
	UISupervisor stopUIProcess.
	w _ WorldMorph newWorld.
	w
		"recreateDefaultDesktop;"
		backgroundImageData: ba;
		addAllMorphs: roots;
		showTaskbar.
	UISupervisor spawnNewMorphicProcessFor: w.
	'Please do [Install New Updates] again.' print.
] fork.
!


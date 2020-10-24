'From Cuis 5.0 [latest update: #4418] on 22 October 2020 at 12:37:46 pm'!

!WorldMorph methodsFor: 'accessing' stamp: 'jmv 10/22/2020 12:31:25'!
handlesKeyboard

	^ true ! !

!WorldMorph methodsFor: 'change reporting' stamp: 'jmv 10/22/2020 12:23:07'!
addedMorph: aMorph
	"Notify the receiver that the given morph was just added."
	super addedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasOpened: aMorph ]! !

!WorldMorph methodsFor: 'change reporting' stamp: 'jmv 10/22/2020 12:33:24'!
removedMorph: aMorph
	"Notify the receiver that aMorph was just removed from its children"
	super removedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasDeleted: aMorph ]! !

!WorldMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 10/22/2020 12:25:35'!
allowsFilesDrop

	^ true! !

!WorldMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 10/22/2020 12:25:43'!
dropFiles: aDropFilesEvent

	(DropFilesAction for: aDropFilesEvent) value! !

!WorldMorph methodsFor: 'event handling' stamp: 'jmv 10/22/2020 12:20:47'!
mouseButton2Activity

	^self invokeWorldMenu! !

!WorldMorph methodsFor: 'events' stamp: 'jmv 10/22/2020 12:32:01'!
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

!WorldMorph methodsFor: 'events' stamp: 'jmv 10/22/2020 12:35:57'!
windowEvent: aMorphicEvent

	aMorphicEvent windowEventType == #windowClose
		ifTrue: [
			^TheWorldMenu basicNew quitSession]
! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 10/22/2020 12:32:35'!
mainLoop

	
	self clearWaitDelay.
	self setCanvas.
	[
		self doOneCycle.
		Processor yield.
		true ]
			whileTrue: []! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 10/22/2020 12:37:28'!
runProcess
	
	| process |
	
	process _ [ self mainLoop ] newProcess.
	process
		priority: Processor userSchedulingPriority;
		name: 'Morphic UI';
		animatedUI: self.
	
	^ process! !

!WorldMorph methodsFor: 'menu & halo' stamp: 'jmv 10/22/2020 12:22:45'!
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

!WorldMorph methodsFor: 'object serialization' stamp: 'jmv 10/22/2020 12:32:58'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	| dp |
	dp _ DiskProxy global: #Smalltalk selector: #runningWorld args: #().
	refStrm replace: self with: dp.
	^ dp! !

!WorldMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 10/22/2020 12:22:24'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect _ Rectangle center: aPoint extent: aMorph morphExtent.
	delta _ trialRect amountToTranslateWithin: self displayBounds.
	self addMorph: aMorph.
	aMorph morphPositionInWorld: trialRect origin + delta.! !

!WorldMorph methodsFor: 'ui services' stamp: 'jmv 10/22/2020 12:34:19'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	"This is deprecated because using it requires blocking the user interface until the user provides a response. Please use the variants that are not modal."
	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock! !

!WorldMorph methodsFor: 'ui services' stamp: 'jmv 10/22/2020 12:34:32'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock! !

!WorldMorph methodsFor: 'windows' stamp: 'jmv 10/22/2020 12:28:19'!
findATranscript
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self
		findAWindowSatisfying: [ :aWindow | aWindow model == Transcript]
		orMakeOneUsing: [ TranscriptWindow openTranscript ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:26:09'!
bringWindowsFullOnscreen
	"Make ever SystemWindow on the desktop be totally on-screen, whenever possible."
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ]) do: [ :each |
		each makeMeFullyVisible ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:26:30'!
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

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:23:43'!
collapseNonWindows
	self allNonWindowRelatedSubmorphs do: [ :m |
		m collapse]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:23:50'!
deleteNonWindows
	(SelectionMenu confirm:
'Do you really want to discard all objects
that are not in windows?')
		ifFalse: [^ self].

	self allNonWindowRelatedSubmorphs do: [:m |
		m delete ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:27:41'!
findAChangeSorter: evt
	"Locate a change sorter, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model isMemberOf: ChangeSorter]
		orMakeOneUsing: [ ChangeSorterWindow open: ChangeSorter new label: nil ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:27:48'!
findAFileList: evt
	"Locate a file list, open it, and bring it to the front.
	Create one if necessary, respecting the Preference."
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == FileList ]
		orMakeOneUsing: [
			FileListWindow openFileList ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:27:57'!
findAMessageNamesWindow: evt
	"Locate a MessageNames tool, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == MessageNames ]
		orMakeOneUsing: [
			MessageNamesWindow open: MessageNames new label: 'Message Names' ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:28:13'!
findATranscript: evt
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self findATranscript! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:28:35'!
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

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:28:49'!
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

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:28:59'!
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

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:29:06'!
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

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 10/22/2020 12:20:57'!
invokeWorldMenu
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed"

	| menu |
	menu _ (TheWorldMenu new 
		world: self
		hand: self activeHand) buildWorldMenu.
	menu addTitle: Preferences desktopMenuTitle.
	menu popUpInWorld: self! !

!WorldMorph methodsFor: 'world state' stamp: 'jmv 10/22/2020 12:24:00'!
allNonWindowRelatedSubmorphs
	"Answer all non-window submorphs that are not flap-related"

	^submorphs 
		reject: [ :m | (m is: #SystemWindow) or: [ m is: #TaskbarMorph ] ]! !

!WorldMorph methodsFor: 'world state' stamp: 'jmv 10/22/2020 12:27:17'!
deleteAllHalos
	self haloMorphs do:
		[ :m | m delete]! !

!WorldMorph methodsFor: 'world state' stamp: 'jmv 10/22/2020 12:30:43'!
fullRepaintNeeded
	self redrawNeeded.
	SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ifTrue: [ w makeMeVisible ].
			false ]! !

!WorldMorph methodsFor: 'world state' stamp: 'jmv 10/22/2020 12:27:23'!
haloMorphs
	^ self hands collect:[:h| h halo] thenSelect:[:halo| halo notNil]! !

!methodRemoval: PasteUpMorph #findWindow: stamp: 'jmv 10/22/2020 12:29:06'!
PasteUpMorph removeSelector: #findWindow:!
!methodRemoval: PasteUpMorph #closeUnchangedWindows stamp: 'jmv 10/22/2020 12:26:30'!
PasteUpMorph removeSelector: #closeUnchangedWindows!
!methodRemoval: PasteUpMorph #findATranscript: stamp: 'jmv 10/22/2020 12:28:13'!
PasteUpMorph removeSelector: #findATranscript:!
!methodRemoval: PasteUpMorph #keyStroke: stamp: 'jmv 10/22/2020 12:32:01'!
PasteUpMorph removeSelector: #keyStroke:!
!methodRemoval: PasteUpMorph #addedMorph: stamp: 'jmv 10/22/2020 12:23:07'!
PasteUpMorph removeSelector: #addedMorph:!
!methodRemoval: PasteUpMorph #mouseButton2Activity stamp: 'jmv 10/22/2020 12:20:47'!
PasteUpMorph removeSelector: #mouseButton2Activity!
!methodRemoval: PasteUpMorph #findATranscript stamp: 'jmv 10/22/2020 12:28:19'!
PasteUpMorph removeSelector: #findATranscript!
!methodRemoval: PasteUpMorph #fullRepaintNeeded stamp: 'jmv 10/22/2020 12:30:43'!
PasteUpMorph removeSelector: #fullRepaintNeeded!
!methodRemoval: PasteUpMorph #request:initialAnswer:orCancel: stamp: 'jmv 10/22/2020 12:34:19'!
PasteUpMorph removeSelector: #request:initialAnswer:orCancel:!
!methodRemoval: PasteUpMorph #windowEvent: stamp: 'jmv 10/22/2020 12:35:57'!
PasteUpMorph removeSelector: #windowEvent:!
!methodRemoval: PasteUpMorph #deleteAllHalos stamp: 'jmv 10/22/2020 12:27:17'!
PasteUpMorph removeSelector: #deleteAllHalos!
!methodRemoval: PasteUpMorph #request:initialAnswer:verifying:do:orCancel: stamp: 'jmv 10/22/2020 12:34:32'!
PasteUpMorph removeSelector: #request:initialAnswer:verifying:do:orCancel:!
!methodRemoval: PasteUpMorph #findDirtyWindows: stamp: 'jmv 10/22/2020 12:28:59'!
PasteUpMorph removeSelector: #findDirtyWindows:!
!methodRemoval: PasteUpMorph #mainLoop stamp: 'jmv 10/22/2020 12:32:35'!
PasteUpMorph removeSelector: #mainLoop!
!methodRemoval: PasteUpMorph #dropFiles: stamp: 'jmv 10/22/2020 12:25:43'!
PasteUpMorph removeSelector: #dropFiles:!
!methodRemoval: PasteUpMorph #handlesKeyboard stamp: 'jmv 10/22/2020 12:31:25'!
PasteUpMorph removeSelector: #handlesKeyboard!
!methodRemoval: PasteUpMorph #findAMessageNamesWindow: stamp: 'jmv 10/22/2020 12:27:57'!
PasteUpMorph removeSelector: #findAMessageNamesWindow:!
!methodRemoval: PasteUpMorph #collapseNonWindows stamp: 'jmv 10/22/2020 12:23:43'!
PasteUpMorph removeSelector: #collapseNonWindows!
!methodRemoval: PasteUpMorph #findAChangeSorter: stamp: 'jmv 10/22/2020 12:27:41'!
PasteUpMorph removeSelector: #findAChangeSorter:!
!methodRemoval: PasteUpMorph #findAWindowSatisfying:orMakeOneUsing: stamp: 'jmv 10/22/2020 12:28:35'!
PasteUpMorph removeSelector: #findAWindowSatisfying:orMakeOneUsing:!
!methodRemoval: PasteUpMorph #findAFileList: stamp: 'jmv 10/22/2020 12:27:48'!
PasteUpMorph removeSelector: #findAFileList:!
!methodRemoval: PasteUpMorph #findDirtyBrowsers: stamp: 'jmv 10/22/2020 12:28:49'!
PasteUpMorph removeSelector: #findDirtyBrowsers:!
!methodRemoval: PasteUpMorph #activeHand stamp: 'jmv 10/22/2020 12:20:12'!
PasteUpMorph removeSelector: #activeHand!
!methodRemoval: PasteUpMorph #haloMorphs stamp: 'jmv 10/22/2020 12:27:23'!
PasteUpMorph removeSelector: #haloMorphs!
!methodRemoval: PasteUpMorph #deleteNonWindows stamp: 'jmv 10/22/2020 12:23:50'!
PasteUpMorph removeSelector: #deleteNonWindows!
!methodRemoval: PasteUpMorph #invokeWorldMenu stamp: 'jmv 10/22/2020 12:20:57'!
PasteUpMorph removeSelector: #invokeWorldMenu!
!methodRemoval: PasteUpMorph #addMorph:centeredNear: stamp: 'jmv 10/22/2020 12:22:24'!
PasteUpMorph removeSelector: #addMorph:centeredNear:!
!methodRemoval: PasteUpMorph #objectForDataStream: stamp: 'jmv 10/22/2020 12:32:58'!
PasteUpMorph removeSelector: #objectForDataStream:!
!methodRemoval: PasteUpMorph #runProcess stamp: 'jmv 10/22/2020 12:37:28'!
PasteUpMorph removeSelector: #runProcess!
!methodRemoval: PasteUpMorph #bringWindowsFullOnscreen stamp: 'jmv 10/22/2020 12:26:09'!
PasteUpMorph removeSelector: #bringWindowsFullOnscreen!
!methodRemoval: PasteUpMorph #allowsFilesDrop stamp: 'jmv 10/22/2020 12:25:35'!
PasteUpMorph removeSelector: #allowsFilesDrop!
!methodRemoval: PasteUpMorph #allNonWindowRelatedSubmorphs stamp: 'jmv 10/22/2020 12:24:00'!
PasteUpMorph removeSelector: #allNonWindowRelatedSubmorphs!
!methodRemoval: PasteUpMorph #addWorldHaloMenuItemsTo:hand: stamp: 'jmv 10/22/2020 12:22:45'!
PasteUpMorph removeSelector: #addWorldHaloMenuItemsTo:hand:!
!methodRemoval: PasteUpMorph #removedMorph: stamp: 'jmv 10/22/2020 12:33:24'!
PasteUpMorph removeSelector: #removedMorph:!

!PasteUpMorph reorganize!
('accessing' color:)
('caching' releaseCachedState)
('drawing' drawOn:)
('dropping/grabbing' allowsMorphDrop allowsSubmorphDrag)
('events' mouseButton1Down:localPosition:)
('event handling testing' handlesMouseDown:)
('event handling' wantsWindowEvent:)
('geometry' fontPreferenceChanged privateExtent:)
('initialization' defaultBorderColor defaultBorderWidth defaultColor)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('stepping and presenter' wantsSteps)
('testing' isReallyVisible)
('halos and balloon help' wantsHaloHandleWithSelector:inHalo:)
!


!WorldMorph reorganize!
('accessing' handlesKeyboard)
('alarms' addAlarm:withArguments:for:at: adjustAlarmTimes: alarmSortBlock removeAlarm:for: triggerAlarmsBefore:)
('caching' releaseCachedState)
('canvas' canvas clearCanvas ensureNonDisplayCanvas recordDamagedRect:for: setCanvas setCanvas:)
('change reporting' addedMorph: invalidateDisplayRect:fromSubmorph:for: removedMorph:)
('classification' isWorldMorph)
('defaul desktop' recreateDefaultDesktop tearDownDesktop)
('drawing' displayWorld drawOn:)
('dropping/grabbing' allowsFilesDrop dropFiles:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('event handling' mouseButton2Activity wantsWindowEvent:)
('events' click:localPosition: keyStroke: windowEvent:)
('geometry' displayBounds externalizeDisplayBounds: externalizeToWorld: internalizeFromWorld: morphPositionInWorld privateExtent: viewBox)
('halos and balloon help' deleteBalloonTarget: wantsHaloHandleWithSelector:inHalo:)
('hands' activeHand hands handsDo: handsReverseDo: selectHandsToDrawForDamage:)
('initialization' clearWaitDelay initialize stepListSortBlock)
('interaction loop' mainLoop runProcess)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand:)
('misc' buildMagnifiedBackgroundImage fillRects: restoreDisplay)
('object serialization' objectForDataStream:)
('printing' printOn:)
('stepping' cleanseStepList runLocalStepMethods: runStepMethods startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('structure' world)
('submorphs-accessing' allMorphsDo: canHandle: firstHand)
('submorphs-add/remove' addMorph:centeredNear:)
('taskbar' hideTaskbar showTaskbar taskbar taskbarDeleted)
('testing' isReallyVisible)
('ui services' request:initialAnswer:orCancel: request:initialAnswer:verifying:do:orCancel:)
('update cycle' checkIfUpdateNeeded displayWorldSafely doOneCycle doOneCycleNow doOneMinimalCycleNow handleFatalDrawingError: tryDeferredUpdatingAndSetCanvas whenUIinSafeState:)
('windows' findATranscript)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu restoreAll)
('world state' allNonWindowRelatedSubmorphs deleteAllHalos fullRepaintNeeded haloMorphs)
!


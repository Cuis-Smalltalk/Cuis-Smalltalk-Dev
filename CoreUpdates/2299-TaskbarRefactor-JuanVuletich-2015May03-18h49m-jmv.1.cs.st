'From Cuis 4.2 of 25 July 2013 [latest update: #2299] on 3 May 2015 at 7:09:27.908583 pm'!
!classDefinition: #TaskbarTask category: #'Taskbar-Core'!
Switch subclass: #TaskbarTask
	instanceVariableNames: 'morph taskbar '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Taskbar-Core'!
!classDefinition: #PasteUpMorph category: #'Morphic-Worlds'!
BorderedRectMorph subclass: #PasteUpMorph
	instanceVariableNames: 'worldState backgroundImage backgroundImageData taskbar '
	classVariableNames: 'WindowEventHandler '
	poolDictionaries: ''
	category: 'Morphic-Worlds'!
!classDefinition: #Taskbar category: #'Taskbar-Core'!
LayoutMorph subclass: #Taskbar
	instanceVariableNames: 'dashBoard viewBox scale '
	classVariableNames: 'SoleInstance '
	poolDictionaries: ''
	category: 'Taskbar-Core'!
!classDefinition: #WorldState category: #'Morphic-Worlds'!
Object subclass: #WorldState
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime lastStepMessage lastCycleTime alarms lastAlarmTime remoteServer drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent activeHand world'
	classVariableNames: 'DeferredUIMessages'
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!TaskbarTask methodsFor: 'accessing' stamp: 'jmv 5/3/2015 19:04'!
taskbar: aTaskbar
	taskbar _ aTaskbar! !


!TaskbarTask class methodsFor: 'instance creation' stamp: 'jmv 5/3/2015 19:04'!
forTaskbar: aTaskbar
	^self new taskbar: aTaskbar! !


!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 5/3/2015 18:58'!
hideTaskbar
	taskbar ifNotNil: [ :tb | tb delete ]! !

!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 5/3/2015 19:08'!
isCollapsed: aWindow

	^ taskbar
		ifNil: [ false ]
		ifNotNil: [ :tb | tb isCollapsed: aWindow ]! !

!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 5/3/2015 19:01'!
minimize: aMorph

	self showTaskbar.
	taskbar minimize: aMorph! !

!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 5/3/2015 19:06'!
restore: aMorph

	taskbar
		ifNotNil: [ :tb | tb restore: aMorph ]! !

!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 5/3/2015 18:59'!
showTaskbar

	taskbar ifNil: [
		taskbar _ Taskbar newRow.
		taskbar openInWorld: self ]! !

!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 5/3/2015 18:57'!
taskbar
	^taskbar! !

!PasteUpMorph methodsFor: 'taskbar' stamp: 'jmv 5/3/2015 18:57'!
taskbar: aTaskbar
	taskbar _ aTaskbar! !


!TaskbarTask methodsFor: 'commands' stamp: 'jmv 5/3/2015 19:05'!
minimize: aMorph

	| button imageForm |

	morph _ aMorph.

	self turnOn.

	button _ PluggableButtonMorph
			model: self
			stateGetter: nil
			action: #switch.
	imageForm _ aMorph imageForm: 32.
	button
		color: taskbar color;
		icon: imageForm;
		setBalloonText: morph label.
	morph visible: false.
			
	^button! !

!TaskbarTask methodsFor: 'commands' stamp: 'jmv 5/3/2015 19:05'!
restore

	morph visible: true.
	(taskbar buttonFor: morph) ifNotNil: [ :button |
		button delete ]! !


!Morph methodsFor: 'menus' stamp: 'jmv 5/3/2015 19:02'!
collapse
	"If taskbar not visible, just hide."

	self world minimize: self! !

!Morph methodsFor: 'menus' stamp: 'jmv 5/3/2015 19:06'!
expand
	
	self comeToFront.
	self world restore: self! !

!Morph methodsFor: 'testing' stamp: 'jmv 5/3/2015 19:08'!
isCollapsed

	^ self world isCollapsed: self! !


!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 5/3/2015 19:07'!
restoreAll
	"Expand all windows to their previous state"
	taskbar
		ifNotNil: [ :tb | tb restoreAll ]! !


!Taskbar methodsFor: 'change reporting' stamp: 'jmv 5/3/2015 19:08'!
delete

	| w |
	self restoreAll.
	super delete.
	w _ self runningWorld.
	Display removeActionsWithReceiver: self.
	w taskbar: nil! !

!Taskbar methodsFor: 'commands' stamp: 'jmv 5/3/2015 19:05'!
minimize: aMorph
	viewBox
		addMorph: ((TaskbarTask forTaskbar: self) minimize: aMorph)
		fixedWidth: self defaultHeight! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/3/2015 19:00'!
preferencesMenu
	"Build the preferences menu for the world."

	^self fillIn: (self menu: 'Preferences...') from: {
		{'Focus follows mouse'. {Preferences. #enableFocusFollowsMouse}. 'At all times, make the active window and widget the one on which the mouse is located.'}.
		{'Click to focus'. {Preferences. #disableFocusFollowsMouse}. 'At all times, make the active window and widget the one where the mouse was clicked.'}.
		{'Font Sizes...' . {Theme . #changeFontSizes} . 'use larger or smaller text and widgets'}.
		{'Icons...' . {Theme . #changeIcons} . 'show more or less icons.'}.
		{'Themes...' . {Theme . #changeTheme} . 'switch to another theme.'}.
		nil.
		{'Show taskbar' . {myWorld . #showTaskbar} . 'show the taskbar'}.
		{'Hide taskbar' . {myWorld . #hideTaskbar} . 'hide the taskbar'}.
		nil.
		{'Full screen on' . { self . #fullScreenOn} . 'puts you in full-screen mode, if not already there.'}.
		{'Full screen off' . { self . #fullScreenOff} . 'if in full-screen mode, takes you out of it.'}.
		nil.
		{'Set display depth...' . {self. #setDisplayDepth} . 'choose how many bits per pixel.'}.
		{'Set desktop color...' . {self. #changeBackgroundColor} . 'choose a uniform color to use as desktop background.'}.
		nil.
		{'Set Code Author...'. {Utilities. #setAuthor}. 'supply initials to be used to identify the author of code and other content.'}.
		{'All preferences...'. {Preferences. #openPreferencesInspector}. 'view and change various options.'}.
	}! !


!Theme class methodsFor: 'class initialization' stamp: 'jmv 5/3/2015 19:00'!
currentTheme: aTheme

	CurrentTheme := aTheme new.
	SHTextStylerST80 initialize.
	self runningWorld ifNotNil: [ :w |
		w backgroundImage ifNil: [
			w color: CurrentTheme background ]].
	SystemWindow initialize.
	BitBltCanvas releaseClassCachedState.
	self runningWorld ifNotNil: [ :w |
		w hideTaskbar; showTaskbar.
		w restoreMorphicDisplay ].
	
	^ CurrentTheme! !

!methodRemoval: WorldState #taskba!
WorldState removeSelector: #taskba!
!methodRemoval: WorldState #taskba4!
WorldState removeSelector: #taskba4!
!methodRemoval: WorldState #taskbar!
WorldState removeSelector: #taskbar!
!methodRemoval: WorldState #taskbar:!
WorldState removeSelector: #taskbar:!

!WorldState reorganize!
('alarms' addAlarm:withArguments:for:at: adjustAlarmTimes: alarmSortBlock alarms removeAlarm:for: triggerAlarmsBefore:)
('canvas' canvas clearCanvas doFullRepaint ensureNonDisplayCanvas recordDamagedRect: setCanvas: viewBox viewBox:)
('hands' activeHand hands handsDo: handsReverseDo: selectHandsToDrawForDamage:)
('initialization' clearWaitDelay initialize stepListSortBlock world:)
('stepping' cleanseStepListForWorld: runLocalStepMethodsIn: runStepMethodsIn: startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('update cycle' checkIfUpdateNeeded displayWorldSafely: doOneCycleFor: doOneCycleNowFor: handleFatalDrawingError: tryDeferredUpdatingAndSetCanvasFor:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('drawing' displayWorld:submorphs: drawInvalidAreasWorld:submorphs: simpleDrawInvalidAreasWorld:submorphs:)
('accessing')
!

!methodRemoval: Taskbar class #clearSoleInstance!
Taskbar class removeSelector: #clearSoleInstance!
!methodRemoval: Taskbar class #hideTaskbar!
Taskbar class removeSelector: #hideTaskbar!
!methodRemoval: Taskbar class #isCollapsed:!
Taskbar class removeSelector: #isCollapsed:!
!methodRemoval: Taskbar class #minimize:!
Taskbar class removeSelector: #minimize:!
!methodRemoval: Taskbar class #releaseClassState!
Taskbar class removeSelector: #releaseClassState!
!methodRemoval: Taskbar class #restore:!
Taskbar class removeSelector: #restore:!
!methodRemoval: Taskbar class #restoreAll!
Taskbar class removeSelector: #restoreAll!
!methodRemoval: Taskbar class #showTaskbar!
Taskbar class removeSelector: #showTaskbar!
!methodRemoval: Taskbar class #singleton!
Taskbar class removeSelector: #singleton!
!classDefinition: #Taskbar category: #'Taskbar-Core'!
LayoutMorph subclass: #Taskbar
	instanceVariableNames: 'dashBoard viewBox scale'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Taskbar-Core'!
!classDefinition: #PasteUpMorph category: #'Morphic-Worlds'!
BorderedRectMorph subclass: #PasteUpMorph
	instanceVariableNames: 'worldState backgroundImage backgroundImageData taskbar'
	classVariableNames: 'WindowEventHandler'
	poolDictionaries: ''
	category: 'Morphic-Worlds'!

!PasteUpMorph reorganize!
('accessing' activeHand color:)
('alarms-scheduler' addAlarm:withArguments:for:at: removeAlarm:for:)
('caching' releaseCachedState)
('change reporting' invalidateDisplayRect:from: redrawNeeded)
('classification' isWorldMorph)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: allowsMorphDrop allowsSubmorphDrag)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('events' click:localPosition: mouseButton1Down:localPosition: windowEvent:)
('event handling testing' handlesMouseDown:)
('event handling' mouseButton2Activity wantsWindowEvent: windowEventHandler)
('geometry' externalizeDisplayBounds: externalizeToWorld: internalizeFromWorld: morphPositionInWorld privateExtent:)
('initialization' clearCanvas clearWaitDelay defaultBorderColor defaultBorderWidth defaultColor)
('interaction loop' doOneCycleNow)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand: deleteBalloonTarget:)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('printing' printOn:)
('project state' canvas firstHand hands handsDo: handsReverseDo: viewBox viewBox:)
('stepping' cleanseStepList runStepMethods startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('stepping and presenter' wantsSteps)
('structure' world)
('submorphs-accessing' allMorphsDo:)
('submorphs-add/remove' addAllMorphs:)
('testing' isReallyVisible stepTime)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu restoreAll)
('world state' addMorph:centeredNear: allNonFlapRelatedSubmorphs deleteAllHalos displayWorld displayWorldSafely doOneCycle fillRects:color: flashRects:color: fullRepaintNeeded haloMorphs privateOuterDisplayWorld restoreMorphicDisplay startSteppingSubmorphsOf: worldState:)
('halos and balloon help' wantsHaloHandleWithSelector:inHalo:)
('object serialization' objectForDataStream:)
('windows' findATranscript)
('taskbar' hideTaskbar isCollapsed: minimize: restore: showTaskbar taskbar taskbar:)
!


!TaskbarTask class reorganize!
('instance creation' forTaskbar:)
!

!classDefinition: #TaskbarTask category: #'Taskbar-Core'!
Switch subclass: #TaskbarTask
	instanceVariableNames: 'morph taskbar'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Taskbar-Core'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points.
Be sure to put any further comments in double-quotes, like this one."
Display removeActionsWithReceiver: Taskbar.
Taskbar allInstancesDo: [ :tb | tb delete ].
self runningWorld showTaskbar
'From Cuis 4.2 of 25 July 2013 [latest update: #1955] on 5 February 2014 at 11:10:06.818454 pm'!

!MorphicAlarm class methodsFor: 'instance creation' stamp: 'jmv 2/2/2014 23:01'!
scheduleReceiver: aTarget selector: aSelector arguments: argArray at: scheduledTime

	^ (self receiver: aTarget selector: aSelector arguments: argArray)
		scheduledTime: scheduledTime! !


!StepMessage class methodsFor: 'instance creation' stamp: 'jmv 2/2/2014 23:06'!
scheduleReceiver: aTarget selector: aSelector at: scheduledTime stepTime: stepTime

	^ (self receiver: aTarget selector: aSelector arguments: nil)
		scheduledTime: scheduledTime;
		stepTime: stepTime! !


!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:20'!
startStepping: aSelector
	"Start getting sent the 'step' message.
	| m |
	m _ RectangleLikeMorph new..
	m openInWorld.
	m startStepping: [self print]
	Esto va a requerir que pueda pasar un bloque....
	"

	self startStepping: aSelector stepTime: nil! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:24'!
startStepping: aSelector in: millisecs stepTime: stepTime
	"Start stepping the receiver"

	self world ifNotNil: [ :w |
		w
			startStepping: self
			at: Time localMillisecondClock+millisecs
			selector: aSelector
			stepTime: stepTime.
		"?"
		"self redrawNeeded" ]! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:23'!
startStepping: aSelector stepTime: stepTime
	"Start stepping the receiver"

	self startStepping: aSelector in: 0 stepTime: stepTime! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:46'!
startSteppingStepTime: stepTime
	"Start stepping the receiver"

	self startStepping: #stepAt: in: 0 stepTime: stepTime! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:51'!
stopStepping: aSelector
	"Stop getting sent the given message."

	self world ifNotNil: [ :w |
		 w stopStepping: self selector: aSelector ]! !


!PasteUpMorph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:43'!
stopSteppingMorph: aMorph
	"Remove the given morph from the step list."

	worldState stopSteppingMorph: aMorph
! !


!ObjectExplorerWindow methodsFor: 'menu commands' stamp: 'jmv 2/2/2014 22:47'!
monitor: anObjectExplorerWrapper
	"Start stepping and watching the given wrapper for changes."

	anObjectExplorerWrapper ifNotNil: [
		model monitorList at: anObjectExplorerWrapper put: anObjectExplorerWrapper asString.
		self startSteppingStepTime: 200 ]! !

!ObjectExplorerWindow methodsFor: 'menu commands' stamp: 'jmv 2/2/2014 22:47'!
stopMonitoring

	model stopMonitoring.
	self stopStepping! !

!ObjectExplorerWindow methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:48'!
stepAt: time

	model step.	
	model monitorList isEmpty ifTrue: [
		self stopStepping ]! !


!WorldState methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:50'!
stopSteppingMorph: aMorph
	"Remove the given morph from the step list."
	lastStepMessage ifNotNil:[
		(lastStepMessage receiver == aMorph) ifTrue: [ lastStepMessage _ nil ]].
	stepList removeAll: (stepList select: [ :stepMsg | stepMsg receiver == aMorph])! !


!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 2/2/2014 22:52'!
step
	"If there's anything in my monitor list, see if the strings have changed."
	| string changes |
	changes _ false.
	self monitorList keysAndValuesDo: [ :k :v |
		k ifNotNil: [
			k refresh.
			(string _ k asString) ~= v ifTrue: [
				self monitorList at: k put: string. changes _ true ]]].
	changes ifTrue: [ | sel |
		sel _ currentSelection.
		self changed: #getList.
		self noteNewSelection: sel ]! !

!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 2/2/2014 22:41'!
stopMonitoring
	monitorList _ nil! !


!MorphicAlarm methodsFor: 'evaluating' stamp: 'jmv 2/5/2014 23:09'!
valueAtTime: millisecondClock

	|  nArgs |
	numArgs ifNil: [numArgs _ selector numArgs].
	nArgs _ arguments ifNil: [0] ifNotNil: [arguments size].
	nArgs = numArgs ifTrue: [
		"Ignore extra argument"
		^self value ].
	^arguments
		ifNil: [ receiver perform: selector with: millisecondClock]
		ifNotNil: [ receiver perform: selector withArguments: (arguments copyWith: millisecondClock) ]! !


!Morph methodsFor: 'events-alarms' stamp: 'jmv 2/2/2014 23:03'!
addAlarm: aSelector after: delayTime
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: nil after: delayTime! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 2/2/2014 22:19'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."
	aMouseButtonEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		^self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition ].

	self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue:[
		self startStepping: #processMouseStillDown
			in: self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 2/2/2014 22:19'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"System level event handling."

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 2/2/2014 22:45'!
processMouseStillDown
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	self hasMouseFocus
		ifFalse: [
			^self stopStepping: #processMouseStillDown ].
	self mouseStillDown! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 2/2/2014 22:45'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."

	aMouseButtonEvent wasHandled ifTrue: [^self]. "not interested"
	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		^self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		^self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition ].
	
	self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self stopStepping: #processMouseStillDown! !

!Morph methodsFor: 'meta-actions' stamp: 'jmv 2/2/2014 22:44'!
dismissMorph
	| w |
	w _ self world ifNil: [ ^self ].
	w deleteAllHalos; stopSteppingMorph: self.
	self delete! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:21'!
startStepping
	"Start getting sent the 'step' message."
	"que feo. le digo stepAt: que lleva un argumento... Cuando hay un argumento, se le pasa automaticamente el tiempo.
	mhhhhh"

	self startStepping: #stepAt:! !

!Morph methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:51'!
stopStepping
	"Stop getting sent the 'step' message."

	self world ifNotNil: [ :w |
		w stopSteppingMorph: self ]! !


!HierarchicalListMorph methodsFor: 'events-processing' stamp: 'jmv 2/2/2014 22:18'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !


!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 2/2/2014 22:20'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !


!ObjectExplorerWindow methodsFor: 'building menus' stamp: 'jmv 2/2/2014 22:41'!
genericMenu
	"Borrow a menu from my inspector"
	
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: '*nothing selected*'
				target: self
				selector: #yourself]
		ifNotNil: [
			aMenu addList: #(
				('inspect (i)'							inspectSelection)
				('explore (I)'						exploreSelection)
				('basic inspect'						inspectBasic)
				('references finder'					openReferencesFinder)
				-
				('browse full (b)'					browseMethodFull)
				('browse hierarchy (h)'				browseHierarchy)
				('browse protocol (p)'				browseFullProtocol)).
			aMenu addLine;
				add: 'monitor changes'
				target: self
				selector: #monitor:
				argument: model getCurrentSelection.
			model class = ReferencesExplorer ifTrue: [
				aMenu addLine;
					add: 'rescan'
					target: self
					selector: #rescan ]].
	model basicMonitorList isEmptyOrNil
		ifFalse: [
			aMenu addLine;
				add: 'stop monitoring all'
				target: self
				selector: #stopMonitoring ].
	^ aMenu! !


!InnerTextMorph methodsFor: 'events-processing' stamp: 'jmv 2/2/2014 22:18'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Re-implemented to allow for mouse-up move events"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1]! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 2/2/2014 22:18'!
startBlinking
	"And show the cursor"
	pauseBlinking _ true.
	"Start blinking in a short while"
	textComposition ifNotNil: [ textComposition showTextCursor: true ].
	self startStepping: #onBlinkCursor stepTime: 500! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 2/2/2014 22:45'!
stopBlinking
	"And do not show cursor anymore."
	self stopStepping: #onBlinkCursor.
	textComposition ifNotNil: [
		textComposition showTextCursor: false ]! !


!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 2/2/2014 22:20'!
startBlinking
	"And show the cursor"
	pauseBlinking _ true.
	"Start blinking in a short while"
	showTextCursor _ true.
	self startStepping: #onBlinkCursor stepTime: 500! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 2/2/2014 22:45'!
stopBlinking
	"And do not show cursor anymore."
	self stopStepping: #onBlinkCursor.
	"Hide cursor right now if needed"
	showTextCursor ifTrue: [
		showTextCursor _ false.
		textCursorRect ifNotNil: [ :r | self invalidateRect: r ]]! !


!WorldState methodsFor: 'alarms' stamp: 'jmv 2/2/2014 23:01'!
addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime
	"Add a new alarm with the given set of parameters"

	self alarms add: 
		(MorphicAlarm 
			scheduleReceiver: aTarget
			selector: aSelector
			arguments: argArray
			at: scheduledTime)! !

!WorldState methodsFor: 'alarms' stamp: 'jmv 2/2/2014 23:16'!
adjustAlarmTimes: nowTime
	"Adjust the alarm times after some clock weirdness (such as image-startup etc)"
	| deltaTime |
	deltaTime _ nowTime - lastAlarmTime.
	self alarms do: [ :alarm |
		alarm scheduledTime: alarm scheduledTime + deltaTime ]! !

!WorldState methodsFor: 'alarms' stamp: 'jmv 2/5/2014 23:03'!
triggerAlarmsBefore: nowTime
	"Trigger all pending alarms that are to be executed before nowTime."
	| pending |
	nowTime - lastAlarmTime > 10000
		ifTrue: [ self adjustAlarmTimes: nowTime ].
	pending _ self alarms.
	[ pending isEmpty not and: [ pending first scheduledTime < nowTime ]]
		whileTrue: [ pending removeFirst valueAtTime: nowTime ].
	lastAlarmTime _ nowTime! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:44'!
cleanseStepListForWorld: aWorld
	"Remove morphs from the step list that are not in this World.  Often were in a flap that has moved on to another world."

	| deletions morphToStep |
	deletions _ nil.
	stepList do: [:entry |
		morphToStep _ entry receiver.
		morphToStep world == aWorld ifFalse:[
			deletions ifNil: [deletions _ OrderedCollection new].
			deletions addLast: entry]].

	deletions ifNotNil:[
		deletions do: [:entry|
			self stopSteppingMorph: entry receiver]].

	self alarms copy do:[:entry|
		morphToStep _ entry receiver.
		((morphToStep is: #Morph) and: [ morphToStep world == aWorld ]) 
			ifFalse: [ self removeAlarm: entry selector for: entry receiver ]].! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 2/2/2014 23:01'!
startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTimeOrNil
	"Add the given morph to the step list. Do nothing if it is already being stepped."
	"Aca y en todos los relacionados, selector puede ser un bloque"
	"Los arguments, no se si sirven de algo. revisar y limpiar."
	"yo quisiera que nunca haya argumentos... se pasan eventos bogus, basura."

	self stopStepping: aMorph selector: aSelector.
	stepList add: (
		StepMessage 
			scheduleReceiver: aMorph
			selector: aSelector
			at: scheduledTime
			stepTime: stepTimeOrNil)! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 2/2/2014 22:50'!
stopStepping: aMorph selector: aSelector
	"Remove the given morph from the step list."
	lastStepMessage ifNotNil: [
		(lastStepMessage receiver == aMorph and:[lastStepMessage selector == aSelector])
			ifTrue:[lastStepMessage _ nil]].
	stepList removeAll: (stepList select:[:stepMsg| stepMsg receiver == aMorph and: [ stepMsg selector == aSelector ]])! !

!methodRemoval: WorldState #stopStepping:!
WorldState removeSelector: #stopStepping:!
!methodRemoval: PasteUpMorph #stopStepping:!
PasteUpMorph removeSelector: #stopStepping:!
!methodRemoval: Morph #startStepping:at:stepTime:!
Morph removeSelector: #startStepping:at:stepTime:!
!methodRemoval: Morph #startSteppingSelector:!
Morph removeSelector: #startSteppingSelector:!
!methodRemoval: StepMessage class #scheduledAt:stepTime:receiver:selector:arguments:!
StepMessage class removeSelector: #scheduledAt:stepTime:receiver:selector:arguments:!
!methodRemoval: MorphicAlarm class #scheduledAt:receiver:selector:arguments:!
MorphicAlarm class removeSelector: #scheduledAt:receiver:selector:arguments:!
!methodRemoval: ObjectExplorer #monitor:!
ObjectExplorer removeSelector: #monitor:!

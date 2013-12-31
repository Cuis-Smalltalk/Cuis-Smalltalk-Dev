'From Cuis 4.0 of 21 April 2012 [latest update: #1300] on 11 June 2012 at 11:23:07 am'!
!classDefinition: #FrameRateMorph category: #'Morphic-Widgets'!
Morph subclass: #FrameRateMorph
	instanceVariableNames: 'lastStepStamp lastStepDelta meanStepDelta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!FrameRateMorph commentStamp: 'jmv 6/11/2012 10:14' prior: 0!
A very simple morph to demo stepping, and for knowing about stepping (and world update) frame rates.

FrameRateMorph new openInHand!


!AutoCompleterMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 09:58'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!!) don't need to answer true to this message"

	^true! !


!FrameRateMorph methodsFor: 'drawing' stamp: 'jmv 6/11/2012 10:29'!
drawOn: aCanvas
	super drawOn: aCanvas.
	meanStepDelta ifNotNil: [
		aCanvas drawString: lastStepDelta rounded printString at: bounds topLeft font: StrikeFont default color: Color black.
		aCanvas drawString: meanStepDelta rounded printString at: bounds topLeft + (0@14) font: StrikeFont default color: Color black.
		aCanvas drawString: lastStepStamp printString at: bounds topLeft + (0@28) font: StrikeFont default color: Color black ]! !

!FrameRateMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 10:31'!
stepAt: millisecondClockValue

	| n |
	lastStepStamp ifNil: [ lastStepStamp _ millisecondClockValue ].
	lastStepDelta _ millisecondClockValue - lastStepStamp.
	lastStepStamp _ millisecondClockValue.
	"This factor is a damper, to show a sort of mean of the n latest step deltas"
	meanStepDelta
		ifNil: [ meanStepDelta _ 0. n _ 0 ]
		ifNotNil: [
"			n _ (meanStepDelta / lastStepDelta between: 0.5 and: 2)
				ifTrue: [ 10 ]
				ifFalse: [10 ]."
			n _ 20 ].
	meanStepDelta _ meanStepDelta * n + lastStepDelta / (n+1).
	self redrawNeeded ! !

!FrameRateMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 11:20'!
stepTime

	^25! !

!FrameRateMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 09:20'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run"

	^true! !


!MagnifierMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 09:58'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!!) don't need to answer true to this message"

	^true! !


!PasteUpMorph methodsFor: 'stepping and presenter' stamp: 'jmv 6/11/2012 09:59'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!!) don't need to answer true to this message.
	jmv: Not really sure. Sub-world stepping needs some review."

	^true! !


!Taskbar methodsFor: 'stepping' stamp: 'jmv 6/11/2012 09:59'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!!) don't need to answer true to this message"

	^true! !


!UpdatingMenuItemMorph methodsFor: 'stepping and presenter' stamp: 'jmv 6/11/2012 10:03'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!!) don't need to answer true to this message"

	^true! !


!Morph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 09:28'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!!) don't need to answer true to this message"

	^false! !


!AutoCompleterMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 09:17'!
step
	self timeOfLastActivity > self timeout
		ifTrue: [ self delete. completer menuClosed ]
		ifFalse: [self updateColor]! !

!AutoCompleterMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 09:18'!
stepTime 
	^ 100! !


!HaloMorph methodsFor: 'testing' stamp: 'jmv 6/11/2012 11:22'!
stepTime
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!!"
	^ 10! !


!HandleMorph methodsFor: 'testing' stamp: 'jmv 6/11/2012 11:22'!
stepTime
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!!"
	^ 10! !


!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 11:22'!
stepTime
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!!"
	^ 10! !


!MagnifierMorph methodsFor: 'stepping' stamp: 'jmv 6/11/2012 11:22'!
stepTime
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!!"
	^ 10! !

!MagnifierMorph methodsFor: 'menu' stamp: 'jmv 6/11/2012 10:05'!
chooseMagnification: evt
	| handle origin aHand currentMag |
	currentMag _ magnification.
	aHand _ evt ifNil: [ self world activeHand ] ifNotNil: [evt hand].
	origin _ aHand position y.
	handle _ HandleMorph new forEachPointDo:
		[ :newPoint | self magnification: (newPoint y - origin) / 8.0 + currentMag ].
	aHand attachMorph: handle.
	handle startStepping.
	self redrawNeeded. ! !


!SystemWindow methodsFor: 'stepping' stamp: 'jmv 6/11/2012 09:55'!
wantsSteps
	"Return true if the model wants its view to be stepped.  For an open system window, we give the model to offer an opinion"

	^ model wantsStepsIn: self! !


!Taskbar methodsFor: 'initialization' stamp: 'jmv 6/11/2012 09:31'!
initialize

	super initialize.
	self step! !

!Taskbar methodsFor: 'stepping' stamp: 'jmv 6/11/2012 11:20'!
stepTime
	"Update very often."
	^ 20! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 6/11/2012 11:18'!
doOneCycleFor: aWorld
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a MaxCycleLapse delay before the ui gets responsive again."

	| hadAnyEvent thisPause now |
	"Check for unnecessary calls to #millisecondClockValue"
	now _ Time millisecondClockValue.
	alarms ifNotNil: [
		alarms isEmpty not ifTrue: [
			pause _ pause min: (alarms first scheduledTime - now) ]].
	stepList isEmpty not ifTrue: [
		pause _ pause min: stepList first scheduledTime - now ].

	thisPause _ pause.
	"pause is set to very short if we have events already enqueued, so they are processed quickly."
	(Preferences higherPerformance or: [ Sensor eventQueue isEmpty not ]) 
		ifTrue: [ thisPause _ 1 ].
	self interCyclePause: thisPause.

	hadAnyEvent _ self doOneCycleNowFor: aWorld.
	hadAnyEvent
		ifTrue: [  pause _ MinCycleLapse ]
		ifFalse: [
			pause < MaxCycleLapse		"No events processed? Start saving CPU!!"
				ifTrue: [
					pause _ pause * 21//20 ]]! !

!methodRemoval: PolygonMorph #wantsSteps!
PolygonMorph removeSelector: #wantsSteps!
!methodRemoval: HandleMorph #startStepping!
HandleMorph removeSelector: #startStepping!
!methodRemoval: AutoCompleterMorph #initialize!
AutoCompleterMorph removeSelector: #initialize!

!AutoCompleterMorph reorganize!
('actions' end help home moveDown moveUp pageDown pageUp resetMenu)
('initialization' defaultBorderColor defaultBorderWidth defaultColor setCompleter:position:)
('paging' currentPage gotoPage: pageCount)
('private' firstVisible lastVisible visibleItemsCount)
('drawing' drawOn:)
('accessing' itemHeight selected selected:)
('event handling' handlesMouseDown: mouseUp:)
('as yet unclassified' lastActivity stillActive timeOfLastActivity timeout updateColor)
('stepping' step stepTime wantsSteps)
!

!methodRemoval: Morph #start!
Morph removeSelector: #start!

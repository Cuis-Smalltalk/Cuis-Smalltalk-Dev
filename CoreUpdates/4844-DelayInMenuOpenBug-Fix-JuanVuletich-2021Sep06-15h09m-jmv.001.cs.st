'From Cuis 5.0 [latest update: #4843] on 6 September 2021 at 3:12:10 pm'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 9/6/2021 15:06:24'!
isDrawnBySoftware
	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor. This is the case if it (a) it is a remote hand, (b) it is showing a temporary cursor, or (c) it is not empty and there are any visible submorphs. If using the software cursor, ensure that the hardware cursor is hidden."
	"Details:  Return true if this hand has a saved patch to ensure that is is processed by the world. This saved patch will be deleted after one final display pass when it becomes possible to start using the hardware cursor again. This trick gives us one last display cycle to allow us to remove the software cursor from the display."
	"Note. We draw the hand as a regular morph (using #drawOn:), disabling the hardware cursor, when we carry submorphs. The reason is to lock the mouse pointer and the carried morph together. Otherwhise the carried morph would lag behind the mouse pointer.
	This method answers whether the regular #drawOn: drawing mechanism is used for us.
	
	Check senders. Hand drawing is handled explicitly by the world, because the Hand is not a submorph of the world!!"
	| blankCursor |
	(prevFullBounds notNil or: [
		submorphs anySatisfy: [ :ea |
			ea visible ]]) ifTrue: [
		"using the software cursor; hide the hardware one"
		blankCursor _ Cursor cursorAt: #blankCursor.
		Cursor currentCursor == blankCursor ifFalse: [ blankCursor activateCursor ].
		^ true ].
	^ false.! !


!WorldMorph methodsFor: 'hands' stamp: 'jmv 9/6/2021 15:06:31'!
selectHandsToDrawForDamage: damageList
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	| result |
	result _ OrderedCollection new.
	hands do: [:hand |
		hand isDrawnBySoftware ifTrue: [
			hand isRedrawNeeded
				ifTrue: [result add: hand]
				ifFalse: [
					hand displayFullBounds ifNotNil: [ :handBounds |
						(damageList anySatisfy: [ :r | r intersects: handBounds]) ifTrue: [
							result add: hand]]]]].
	^ result! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/6/2021 15:06:36'!
checkIfUpdateNeeded

	self isSubmorphRedrawNeeded ifTrue: [ ^true ].
	damageRecorder updateIsNeeded ifTrue: [^true].
	hands do: [:h | (h isRedrawNeeded | h isSubmorphRedrawNeeded and: [h isDrawnBySoftware]) ifTrue: [^true]].
	^false  "display is already up-to-date"
! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/6/2021 15:10:14'!
doOneCycle
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	(lastCycleHadAnyEvent or: [ deferredUIMessages isEmpty not ])
		ifTrue: [
			pause _ 20.				"This value will only be used later, when there are no more events to serve or deferred UI messages to process."
			wait _ 0.				"Don't wait this time"]
		ifFalse: [
			"wait between 20 and 200 milliseconds"
			(hands anySatisfy: [ :h | h waitingForMoreClicks ])
				ifTrue: [ pause _ 20 ]
				ifFalse: [ pause < 200 ifTrue: [ pause _ pause * 21//20 ] ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].
			wait _ waitUntil - Time localMillisecondClock max: 0 ].
	Preferences serverMode
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait = 0
		ifTrue: [ Processor yield ]
		ifFalse: [
			waitDelay beingWaitedOn
				ifFalse: [ waitDelay setDelay: wait; wait ]
				ifTrue: [
					"If we are called from a different process than that of the main UI, we might be called in the main
					interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
					(Delay forMilliseconds: wait) wait ]].

	"Record start time of this cycle, and do cycle"
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNow.! !

!methodRemoval: HandMorph #needsToBeDrawn stamp: 'jmv 9/6/2021 15:11:24'!
HandMorph removeSelector: #needsToBeDrawn!

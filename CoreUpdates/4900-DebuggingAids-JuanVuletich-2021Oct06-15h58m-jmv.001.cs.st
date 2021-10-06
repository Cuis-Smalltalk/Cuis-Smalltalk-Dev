'From Cuis 5.0 [latest update: #4895] on 6 October 2021 at 4:02:52 pm'!

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/6/2021 16:02:03'!
doOneCycle
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	"
	'Debugging Aid. Declare Delta either as a class variable or as a global variable. Declare delta and r as locals'.
	delta _ Time localMillisecondClock - lastCycleTime.
	r _ 15@1515 extent: 60@30.
	Delta _ Delta ifNil: [ delta ] ifNotNil: [ Delta * 0.9 + (delta * 0.1) ].
	Random next > 0.9 ifTrue: [
		Display fill: r fillColor: Color white.
		(Delta printStringFractionDigits: 1) displayAt: 20@1520.
		Display forceToScreen: r ].
	"
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


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 9/29/2021 10:39:57'!
activeSubclass: aMorphicCanvasSubclass
	"
	self runningWorld canvas class
	MorphicCanvas activeSubclass: BitBltCanvas
	MorphicCanvas activeSubclass: HybridCanvas
	MorphicCanvas activeSubclass: VectorDrawingCanvas
	"
	(#(BitBltCanvas HybridCanvas VectorDrawingCanvas)
		includes: aMorphicCanvasSubclass name) ifFalse: [
			^self error: 'Invalid Canvas class' ].
	ActiveSubclass _ aMorphicCanvasSubclass.
	UISupervisor ui ifNotNil: [ :world |
		world whenUIinSafeState: [
			world setMainCanvas ]].! !


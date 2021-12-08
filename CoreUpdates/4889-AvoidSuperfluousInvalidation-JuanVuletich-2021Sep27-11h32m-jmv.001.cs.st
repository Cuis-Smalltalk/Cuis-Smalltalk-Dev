'From Cuis 5.0 [latest update: #4888] on 27 September 2021 at 11:37:25 am'!

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 9/27/2021 11:30:21'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	| prevTranslation |
	prevTranslation _ location translation.
	location _ location withTranslation: aPoint.
	"Ask if translation effectively changed, after possible conversion to 32 bit Float in AffineTransformation. "
	location translation = prevTranslation ifFalse: [
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].! !


!WorldMorph methodsFor: 'drawing' stamp: 'jmv 9/27/2021 11:37:17'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"repair world's damage on canvas"
	worldDamageRects _ canvas drawWorld: self repair: damageRecorder.
	"
	worldDamageRects do: [ :r | Display border: r width: 3 fillColor: Color random ].	 'Debugging Aid'.
	"

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
	DisplayScreen isDisplayExtentOk ifTrue: [
		Display forceDamageToScreen: allDamage ].

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/27/2021 11:37:03'!
doOneCycle
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	"
	'Debugging Aid'.
	Display fill: (15@1515 extent: 200@30) fillColor: Color white.
	(Time localMillisecondClock - lastCycleTime) printString displayAt: 20@1520.
	Display forceToScreen.
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


!BoxedFloat64 reorganize!
('arithmetic' * + - / arg argument)
('comparing' < <= = > >= ~=)
('mathematical functions' arcTan exp ln primSqrt raisedTo: raisedToInteger: sin timesTwoPower:)
('truncation and round off' exponent fractionPart truncated)
('testing' isDenormalized isFinite isInfinite isNaN sign)
('printing' storeOn:base:)
!


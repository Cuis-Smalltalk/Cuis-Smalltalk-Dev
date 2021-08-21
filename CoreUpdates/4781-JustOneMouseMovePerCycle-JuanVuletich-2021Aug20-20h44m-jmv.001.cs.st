'From Cuis 5.0 [latest update: #4780] on 20 August 2021 at 8:45:36 pm'!

!MorphicEvent methodsFor: 'testing' stamp: 'jmv 8/20/2021 20:44:30'!
isMouseMove
	^false! !


!HandMorph methodsFor: 'event handling' stamp: 'jmv 8/20/2021 20:33:25'!
processEventQueue
	"Process user input events from the local input devices.
	Answer true if any event was handled (but ignore MouseMove)"

	| evt evtBuf type hadAny mcs hadAnyMouseEvent hadAnyMouseMoveEvent |
	mcs _ mouseClickState.
	hadAny _ false.
	hadAnyMouseEvent _ false.
	hadAnyMouseMoveEvent _ false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		type _ evtBuf first.
		evt _ self createEventFrom: evtBuf ofType: type.
		evt ifNotNil: [
			"Finally, handle it. But process only up to one mouseMove per cycle. Discard the rest."
			(evt isMouseMove and: [ hadAnyMouseMoveEvent ]) ifFalse: [
				self startEventDispatch: evt.
				hadAny _ true.
				evt isMouse ifTrue: [
					hadAnyMouseEvent _ true.
					evt isMouseMove ifTrue: [
						hadAnyMouseMoveEvent _ true ]]]]].
	hadAnyMouseEvent ifFalse: [
		mcs 
			ifNotNil: [ 
				"No mouse events during this cycle. Make sure click states time out accordingly"
				mcs
					handleEvent: lastMouseEvent asMouseMove
					from: self ]].
	^hadAny! !

!methodRemoval: KeyboardEvent #isMouseMove stamp: 'jmv 8/20/2021 20:44:42'!
KeyboardEvent removeSelector: #isMouseMove!

'From Cuis 5.0 of 7 November 2016 [latest update: #3544] on 27 December 2018 at 10:29:13 am'!

!HandMorph methodsFor: 'event handling' stamp: 'HAW 12/27/2018 10:28:27'!
createEventFrom: eventBuffer ofType: type

	type = EventSensor eventTypeMouse ifTrue: [ ^self generateMouseEvent: eventBuffer ].
	type = EventSensor eventTypeKeyboard ifTrue: [ ^self generateKeyboardEvent: eventBuffer ].
	type = EventSensor eventTypeWindow ifTrue: [ ^self generateWindowEvent: eventBuffer ].
	type = EventSensor eventTypeDragDropFiles ifTrue: [ ^self generateDropFilesEvent: eventBuffer ].
		
	"All other events are ignored"
	^nil ! !


!HandMorph methodsFor: 'event handling' stamp: 'HAW 12/27/2018 10:28:17'!
processEventQueue
	"Process user input events from the local input devices."

	| evt evtBuf type hadAny mcs |
	mcs _ mouseClickState.
	hadAny := false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		type := evtBuf first.
		evt := self createEventFrom: evtBuf ofType: type.
		evt
			ifNil: [ 
				"I have to consume all eventTypeDragDropFiles of type 2 quicky, that is why
				I check if it was an eventTypeDragDropFiles to continue in the loop - Hernan"
				type ~= EventSensor eventTypeDragDropFiles ifTrue: [^hadAny]]
			ifNotNil: [
				"Finally, handle it"
				self startEventDispatch: evt.
				hadAny := true.
				"For better user feedback, return immediately after a mouse event has been processed."
				evt isMouse ifTrue: [ ^hadAny ]]].
	"note: if we come here we didn't have any mouse events"
	mcs 
		ifNotNil: [ 
			"No mouse events during this cycle. Make sure click states time out accordingly"
			mcs
				handleEvent: (self lastMouseEvent asMouseMove: (Time localMillisecondClock - self lastMouseEventTime max: 0))
				from: self ].
	^hadAny! !


'From Cuis 6.0 [latest update: #5604] on 21 December 2022 at 12:43:00 pm'!

!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 12/21/2022 12:27:53'!
fetchMoreEventsDiscardingMouseEvents: discardMouseEvents
	"Fetch more events from the VM"
	| eventBuffer type |

	"Reset input semaphore so clients can wait for the next events after this one."
	inputSemaphore isSignaled
		ifTrue: [
			hasInputSemaphore _ true.
			inputSemaphore initSignals ].

	"DebuggingAid""
	dd _ Time localMillisecondClock - lastEventPoll.
	"
	"Remember the last time that I checked for events."
	lastEventPoll _ Time localMillisecondClock.

	eventBuffer _ Array new: 8.
	[
		self primGetNextEvent: eventBuffer.
		"DebuggingAid""
		eventBuffer first = 2 ifTrue: [
			k1 _ eventBuffer third.
			u1 _ eventBuffer sixth.
			pressType _ eventBuffer fourth.
			typeS _ 'unknown'.
			pressType = EventSensor eventKeyDown ifTrue: [
				typeS _ #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				typeS _ #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				typeS _ #keystroke].
			dd >100 ifTrue: ['-' print. dd _ 0 ].
			{ k1 < 256 ifTrue: [Character numericValue: k1] ifFalse: [ '[]']. UnicodeCodePoint codePoint: k1. k1. k1 hex. '-----'. 
				UnicodeCodePoint codePoint: u1. u1. u1 hex. '----'. pressType. typeS } print.
			].
		"
		type _ eventBuffer at: 1.
		type = EventSensor eventTypeNone ]
			whileFalse: [
				self
					processSensorEvent: eventBuffer
					discardingMouseEvents: discardMouseEvents ]! !

!EventSensor methodsFor: 'test' stamp: 'jmv 12/21/2022 12:41:28'!
printEventBuffer: evtBuf
	"Print the event buffer, currently only used by the method `test`."

	| type buttons macRomanCode modifiers pressType stamp unicodeCodePoint |
	type _ evtBuf first.
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	type = EventSensor eventTypeMouse
		ifTrue: [ | position |
			position _ evtBuf third @ evtBuf fourth.
			buttons _ evtBuf fifth.
			modifiers _ evtBuf sixth.
			Transcript
				newLine;
				show: 'Mouse';
				show: ' position:', position printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeMouseScroll
		ifTrue: [ | delta |
			delta _ evtBuf third @ evtBuf fourth.
			buttons _ evtBuf fifth.
			modifiers _ evtBuf sixth.
			Transcript
				newLine;
				show: 'Scroll';
				show: ' delta:', delta printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeKeyboard 
		ifTrue: [
			macRomanCode _ evtBuf third.
			unicodeCodePoint _ evtBuf sixth.
			pressType _ evtBuf fourth.
			modifiers _ evtBuf fifth.
			pressType = EventSensor eventKeyDown ifTrue: [
				type _ #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				type _ #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				type _ #keystroke].
			{type. 'unicodeCodePoint:'. unicodeCodePoint. unicodeCodePoint hex. UnicodeCodePoint codePoint: unicodeCodePoint} print.
			Transcript nextPutAll: ' modifiers: ', modifiers printString.
			(modifiers anyMask: 8) ifTrue: [ Transcript nextPutAll: ' [commandWinAlt]' ].
			(modifiers anyMask: 4) ifTrue: [ Transcript nextPutAll: ' [macOption]' ].
			(modifiers anyMask: 2) ifTrue: [ Transcript nextPutAll: ' [control]' ].
			(modifiers anyMask: 1) ifTrue: [ Transcript nextPutAll: ' [shift]' ].
			''print.
			].! !

!EventSensor methodsFor: 'test' stamp: 'jmv 12/21/2022 12:42:55'!
test
	"
	Sensor test

Also useful:
	Sensor kbdTest
	"
	| char lastStamp stamp |
	char _ nil.
	lastStamp _ 0.
	[ char = $x ] whileFalse: [
		Sensor nextEvent ifNotNil: [ :evt |
			stamp _ evt at: 2.
			stamp - lastStamp > 300 ifTrue: [ ' ' print.' ' print ].
			self printEventBuffer: evt.
			Transcript show: evt.
			(evt first = 2 and: [ evt fourth = 0 ]) ifTrue: [
				char _ Character numericValue: evt third ].
			lastStamp _ stamp]]! !


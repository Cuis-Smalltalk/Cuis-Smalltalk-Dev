'From Cuis 4.0 of 3 April 2012 [latest update: #4209] on 11 April 2012 at 9:35:50 pm'!

!HandMorph methodsFor: 'private events' stamp: 'jmv 4/11/2012 21:35'!
generateMouseEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| pos buttons modifiers type trail stamp oldButtons |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ].
	pos := evtBuf third @ evtBuf fourth.
	buttons := evtBuf fifth.
	modifiers := evtBuf sixth.
	type := buttons = 0 
		ifTrue: [
			lastEventBuffer fifth = 0 ifTrue: [#mouseMove] ifFalse: [#mouseUp]]
		ifFalse: [
			lastEventBuffer fifth = 0 
						ifTrue: [#mouseDown]
						ifFalse: [#mouseMove]].
	buttons := buttons bitOr: (modifiers bitShift: 3).
	oldButtons := lastEventBuffer fifth 
				bitOr: (lastEventBuffer sixth bitShift: 3).
	lastEventBuffer := evtBuf.
	type == #mouseMove 
		ifTrue: [
			trail := self mouseTrailFrom: evtBuf.
			^MouseMoveEvent new 
				setType: type
				startPoint: trail first
				endPoint: trail last
				trail: trail
				buttons: buttons
				hand: self
				stamp: stamp].
	^MouseButtonEvent new 
		setType: type
		position: pos
		which: (oldButtons bitXor: buttons)
		buttons: buttons
		hand: self
		stamp: stamp! !


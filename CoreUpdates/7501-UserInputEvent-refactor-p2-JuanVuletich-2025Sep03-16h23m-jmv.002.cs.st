'From Cuis7.5 [latest update: #7495] on 3 September 2025 at 4:24:16 pm'!

!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 9/3/2025 16:23:45'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"
	keyEventType == #keystroke ifTrue: [
		self isFindClassShortcut ifTrue: [ 
			(Preferences at: #classFinder) value.
			^self ].
		self isCloseWindowShortcut ifTrue: [
			self closeCurrentWindowOf: aMorph.
			^self ].
		aMorph processKeystroke: self.
		^self ].
	keyEventType == #keyDown ifTrue: [
		aMorph processKeyDown: self.
		^self ].
	keyEventType == #keyUp ifTrue: [ 
		aMorph processKeyUp: self.
		^self ].
	super sendEventTo: aMorph.! !

!KeyboardEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:23:57'!
isKeyDown
	^ keyEventType == #keyDown! !

!KeyboardEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:23:53'!
isKeyUp
	^ keyEventType == #keyUp! !

!KeyboardEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:23:49'!
isKeystroke
	^ keyEventType == #keystroke! !


!MouseButtonEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:23:33'!
isMouseDown
	^ buttonEventType == #mouseDown! !

!MouseButtonEvent methodsFor: 'testing' stamp: 'jmv 9/3/2025 16:23:30'!
isMouseUp
	^ buttonEventType == #mouseUp! !


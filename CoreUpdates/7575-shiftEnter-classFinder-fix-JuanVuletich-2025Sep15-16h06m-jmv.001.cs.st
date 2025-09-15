'From Cuis7.5 [latest update: #7574] on 15 September 2025 at 4:08:55 pm'!

!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 9/15/2025 16:07:17'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."

	keyEventType == #keystroke ifTrue: [
		(self shiftPressed and: [ self isReturnKey ]) ifTrue: [ 
			(Preferences at: #classFinder) value.
			self wasHandled: true.
			^self ].
		aMorph processKeystroke: self.
		^self ].
	keyEventType == #keyDown ifTrue: [
		aMorph processKeyDown: self.
		^self ].
	keyEventType == #keyUp ifTrue: [ 
		aMorph processKeyUp: self.
		^self ].! !

!methodRemoval: WorldMorph #keyStroke: stamp: 'jmv 9/15/2025 16:06:24'!
WorldMorph removeSelector: #keyStroke:!
!methodRemoval: SystemDictionary #openClassFinder stamp: 'jmv 9/15/2025 16:08:46'!
SystemDictionary removeSelector: #openClassFinder!

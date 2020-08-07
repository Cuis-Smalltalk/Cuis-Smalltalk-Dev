'From Cuis 5.0 [latest update: #4346] on 6 August 2020 at 9:43:13 pm'!

!Morph methodsFor: 'user interface' stamp: 'jmv 8/6/2020 21:41:56'!
activateWindowAndSendTopToBack: aBoolean

	self owningWindow ifNotNil: [ :w |
		w activateAndSendTopToBack: aBoolean]! !


!SystemWindow methodsFor: 'top window' stamp: 'jmv 8/6/2020 21:40:32'!
activateAndSendTopToBack: aBoolean
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	
	self isTopWindow ifTrue: [
		self comeToFront.
		^self ].

	oldTop _ TopWindow.
	TopWindow _ self.
	self redrawNeeded.

	oldTop ifNotNil: [
		oldTop redrawNeeded.
		aBoolean ifTrue: [
			| bottomWindow |
			bottomWindow _ oldTop owner submorphs reverse detect: [:one | one is: #SystemWindow].
			oldTop owner addMorph: oldTop behind: bottomWindow]].

	self comeToFront.

	"Set keyboard focus"
	self world ifNotNil: [ :w |
		w activeHand newKeyboardFocus: self submorphToFocusKeyboard ]! !


!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/6/2020 21:40:56'!
activateNextWindow

	self nextFocusWindow ifNotNil: [ :w |
		w activateAndSendTopToBack: true ]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 8/6/2020 21:41:04'!
activatePreviousWindow

	self previousFocusWindow ifNotNil: [ :w |
		w activateAndSendTopToBack: false ]! !


'From Cuis 5.0 [latest update: #4325] on 1 August 2020 at 6:09:38 pm'!

!SystemWindow methodsFor: 'top window' stamp: 'jmv 8/1/2020 18:08:13'!
activateAndSendTopToBack: aBoolean
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	oldTop _ TopWindow.
	TopWindow _ self.
	self redrawNeeded.

	oldTop ifNotNil: [
		oldTop redrawNeeded.
		aBoolean ifTrue: [
			| bottomWindow |
			bottomWindow _ oldTop owner submorphs reverse detect: [:one | one is: #SystemWindow].
			oldTop owner addMorph: oldTop behind: bottomWindow]].

	owner firstSubmorph == self 
		ifFalse: [
			"Bring me to the top if not already"
			owner addMorphFront: self].

	"Set keyboard focus"
	self world ifNotNil: [ :w |
		w activeHand newKeyboardFocus: self submorphToFocusKeyboard ]! !


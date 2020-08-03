'From Cuis 5.0 [latest update: #4272] on 22 July 2020 at 3:10:31 pm'!

!SystemWindow methodsFor: 'top window' stamp: 'jmv 7/22/2020 15:07:10'!
activateAndSendTopToBack: aBoolean
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	oldTop _ TopWindow.
	TopWindow _ self.

	oldTop ifNotNil: [
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


'From Cuis 4.2 of 25 July 2013 [latest update: #2583] on 18 November 2015 at 3:17:44.034573 am'!

!SystemWindow methodsFor: 'top window' stamp: 'len 11/17/2015 03:19'!
activateAndSendTopToBack: aBoolean
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	self owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	oldTop _ TopWindow.
	TopWindow _ self.

	oldTop ifNotNil: [
		oldTop redrawNeeded.
		aBoolean ifTrue: [
			| bottomWindow |
			bottomWindow _ oldTop owner submorphs reverse detect: [:one | one is: #SystemWindow].
			oldTop owner addMorph: oldTop behind: bottomWindow]].

	self owner firstSubmorph == self 
		ifFalse: [
			"Bring me to the top if not already"
			self owner addMorphFront: self].
	self redrawNeeded.

	"Set keyboard focus"
	self world ifNotNil: [ :w |
		w activeHand newKeyboardFocus: self submorphToFocusKeyboard ]! !


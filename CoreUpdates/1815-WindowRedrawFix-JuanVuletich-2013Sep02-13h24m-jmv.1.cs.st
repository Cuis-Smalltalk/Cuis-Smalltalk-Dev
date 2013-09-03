'From Cuis 4.2 of 25 July 2013 [latest update: #1814] on 2 September 2013 at 1:25:43.949607 pm'!

!SystemWindow methodsFor: 'top window' stamp: 'jmv 9/2/2013 13:24'!
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
			oldTop owner addMorphBack: oldTop ]].

	self owner firstSubmorph == self 
		ifFalse: [
			"Bring me to the top if not already"
			self owner addMorphFront: self].
	self redrawNeeded.

	"Set keyboard focus"
	self world ifNotNil: [ :w |
		w activeHand newKeyboardFocus: self submorphToFocusKeyboard ]! !


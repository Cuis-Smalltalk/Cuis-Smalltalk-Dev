'From Cuis7.9 [latest update: #8131] on 5 August 2026 at 11:27:14 am'!

!SystemWindow methodsFor: 'top window' stamp: 'jmv 5/Aug/2026, 2:27:11 pm (UTC)'!
activateAndSendTopToBack: aBoolean
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	
	self isTopWindow ifTrue: [
		self comeToFront.
		^self ].

	oldTop := TopWindow.
	TopWindow := self.
	self needsRedraw: true.

	oldTop ifNotNil: [
		oldTop invalidateTitleArea.
		aBoolean ifTrue: [
			| bottomWindow |
			bottomWindow := oldTop owner submorphs reverse detect: [:one | one is: #SystemWindow].
			oldTop owner addMorph: oldTop behind: bottomWindow]].

	self comeToFront.

	"Set keyboard focus: #submorphToFocusKeyboard, morph under hand, or window"
	self world ifNotNil: [ :w |
		(Preferences at: #focusFollowsMouse)
			ifTrue: [w activeHand dispatchMouseOverEvent]
			ifFalse: [
				w activeHand newKeyboardFocus:
					(self submorphToFocusKeyboard ifNil: [self])]].! !


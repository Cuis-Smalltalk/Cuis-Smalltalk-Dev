'From Cuis7.9 [latest update: #8074] on 22 July 2026 at 10:38:17 am'!

!SystemWindow methodsFor: 'top window' stamp: 'jmv 22/Jul/2026, 1:38:10 pm (UTC)'!
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
		self submorphToFocusKeyboard
			ifNotNil: [ :fkm |
				w activeHand newKeyboardFocus: fkm ]
			ifNil: [
				(Preferences at: #focusFollowsMouse)
					ifTrue: [w activeHand dispatchMouseOverEvent]
					ifFalse: [w activeHand newKeyboardFocus: self]]].! !


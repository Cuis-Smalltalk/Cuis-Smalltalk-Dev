'From Cuis7.5 [latest update: #7558] on 11 September 2025 at 3:43:02 pm'!

!Morph methodsFor: 'focus handling' stamp: 'jmv 9/11/2025 15:39:38'!
submorphToFocusKeyboard
	"Answer a preferred submorph for initial keyboard focus, if desired."
	^nil! !


!SystemWindow methodsFor: 'top window' stamp: 'jmv 9/11/2025 15:41:02'!
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

	"Set keyboard focus"
	self world ifNotNil: [ :w |
		self submorphToFocusKeyboard
			ifNotNil: [ :fkm | w activeHand newKeyboardFocus: fkm ]
			ifNil: [ w activeHand dispatchMouseOverEvent ]]! !

!methodRemoval: SystemWindow #submorphToFocusKeyboard stamp: 'jmv 9/11/2025 15:42:41'!
SystemWindow removeSelector: #submorphToFocusKeyboard!

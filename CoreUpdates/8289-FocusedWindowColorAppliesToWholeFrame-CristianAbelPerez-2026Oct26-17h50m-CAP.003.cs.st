'From Cuis7.9 [latest update: #8282] on 26 October 2026 at 6:20:45 pm'!

!SystemWindow methodsFor: 'drawing' stamp: 'CAP 26/Oct/2026, 9:16:14 pm (UTC)'!
drawClassicFrameOn: aCanvas color: windowFrameColor
	"Window border encompasses title area. No round corners."

	aCanvas
		fillRectangle: (self localBounds insetBy: 1.5)
		color: color
		borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: windowFrameColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas
		fillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight)
		color: windowFrameColor.! !

!SystemWindow methodsFor: 'drawing' stamp: 'CAP 26/Oct/2026, 8:24:59 pm (UTC)'!
drawRoundedFrameOn: aCanvas color: windowFrameColor

	aCanvas
		drawRoundedFrame: self localBounds
		border: borderWidth
		color: windowFrameColor
		topColor: windowFrameColor
		insideColor: color
		labelHeight: self labelHeight.! !

!SystemWindow methodsFor: 'top window' stamp: 'CAP 26/Oct/2026, 9:16:14 pm (UTC)'!
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
		oldTop invalidateBounds.
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


'From Cuis7.9 [latest update: #7998] on 16 June 2026 at 1:29:42 am'!

!StringRequestMorph methodsFor: 'as yet unclassified' stamp: 'len 16/Jun/2026 01:20:06'!
delete
	prevKeyboardFocus ifNotNil: [
		prevKeyboardFocus world ifNotNil: [ :w |
			w activeHand newKeyboardFocus: prevKeyboardFocus ]].
	super delete! !


!StringRequestMorph methodsFor: 'initialization' stamp: 'len 15/Jun/2026 00:22:11'!
addTextPane
	"Add a text pane and two buttons."
	
	| controlsContainer |
	textMorph := (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			textMorph emptyTextDisplayMessage: msg ].
	textMorph
		hasUnacceptedEdits: true; "Allow accepting initial contents"
		acceptOnCR: true;
		escAction: [ self cancel ].
	(controlsContainer := LinearLayoutMorph newRow color: Color transparent)
		addMorphUseAll: textMorph;
		addMorph: 
			((PluggableButtonMorph model: self action: #ok)
				setBalloonText: 'OK' bold, ' (accept input)';
				icon: Theme current acceptIcon);
		addMorph:
			((PluggableButtonMorph model: self action: #cancel)
				setBalloonText: 'Cancel' bold, '  (discard input)';
				icon: Theme current cancelIcon).
	self addMorphUseAll: controlsContainer! !

!StringRequestMorph methodsFor: 'private' stamp: 'jmv 21/May/2026 16:06:39'!
cancel
	response := ''.
	self delete.
	cancelBlock ifNotNil: [cancelBlock value].! !

!StringRequestMorph methodsFor: 'private' stamp: 'len 16/Jun/2026 01:19:42'!
getUserResponseOrCancel: aBlock
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."
	| world delay done canceled answer |
	world := self world.
	world isNil ifTrue: [^ response asString asByteStringIfAscii ].
	done := false.
	canceled := false.
	(Preferences at: #focusFollowsMouse) ifFalse: [self textBox focusText].
	acceptBlock := [:aString| done := true].
	cancelBlock := [done := true. canceled := true].
	delay := Delay forMilliseconds: 10.
	[done not and: [self isInWorld]] whileTrue: [ world doOneMinimalCycleNow. delay wait ].
	self delete.
	world doOneMinimalCycleNow.
	answer := canceled
		ifTrue: [aBlock value]
		ifFalse: [response asString asByteStringIfAscii].
	^answer! !


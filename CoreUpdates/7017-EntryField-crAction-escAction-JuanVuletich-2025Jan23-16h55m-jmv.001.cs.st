'From Cuis7.3 [latest update: #7004] on 23 January 2025 at 4:56:21 pm'!

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 1/23/2025 16:30:49'!
crAction
	"Return the action to perform when user presses <Return> key"
	^self valueOfProperty: #crAction! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 1/23/2025 16:30:45'!
crAction: aBlock
	"Sets the action to perform when user presses <Return> key"
	self setProperty: #crAction toValue: aBlock! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 1/23/2025 16:39:30'!
escAction
	"Return the action to perform when user presses <Esc> key"

	^self valueOfProperty: #escAction! !

!EntryFieldMorph methodsFor: 'accessing' stamp: 'jmv 1/23/2025 16:39:35'!
escAction: aBlock
	"Sets the action to perform when user presses <Esc> key"

	self setProperty: #escAction toValue: aBlock.! !


!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 1/23/2025 16:39:41'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ 
		self updateFromContents.
		self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
		self redrawNeeded.
		^self ].

	self pauseBlinking.

	"Return - check for special action"
	aKeyboardEvent isReturnKey ifTrue: [
		self crAction ifNotNil: [ :action |
			^action value ]].

	"Esc - check for special action"
	aKeyboardEvent isEsc ifTrue: [			
		self escAction ifNotNil: [ :action |
			^action value ]].

	self handleInteraction: [ self editor processKeystrokeEvent: aKeyboardEvent ].
	self updateFromContents.
	self morphExtent: (self morphExtent max: self minimumExtent). "big enough to fit"
	self redrawNeeded.! !


'From Cuis7.5 [latest update: #7540] on 10 September 2025 at 9:14:47 am'!

!EntryFieldMorph methodsFor: 'blink cursor' stamp: 'jmv 9/9/2025 17:46:17'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking := true.
	self disablesEditing ifFalse: [
		"Show cursor right now if needed"
		showTextCursor ifFalse: [
			showTextCursor := true.
			textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r ]]]! !

!EntryFieldMorph methodsFor: 'events-processing' stamp: 'jmv 9/9/2025 17:45:57'!
keyboardFocusChange: aBoolean
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor ].
			self disablesEditing ifFalse: [
				self startBlinking ]]
		ifFalse: [
			self stopBlinking ].
	self redrawNeeded.! !


!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 9/9/2025 17:47:20'!
keyboardFocusChange: aBoolean
	
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw border feedback"
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor storeSelectionInComposition ].	"Forces install"
			(self disablesEditing not and: [ editor hasSelection not ]) ifTrue: [
				self startBlinking ]]
		ifFalse: [ self stopBlinking ].
	
	(owner notNil and: [ owner drawsKeyboardFocusIndicator ])
		ifTrue: [ owner redrawNeeded ]
		ifFalse: [
			"Selection might be shown differently when focused"
			self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ]]	.! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 9/9/2025 17:49:01'!
selectionChanged
	self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ].
	(self disablesEditing not and: [ editor hasSelection not ])
		ifTrue: [ self hasKeyboardFocus ifTrue: [self startBlinking ]]
		ifFalse: [ self stopBlinking ]! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 9/9/2025 17:47:44'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking := true.
	textComposition ifNotNil: [
		self disablesEditing ifFalse: [
			"Show cursor right now if needed"
			textComposition showTextCursor ifFalse: [
				textComposition showTextCursor: true ]]]! !

!methodRemoval: InnerTextMorph #showsBlinkingCursor stamp: 'jmv 9/10/2025 09:14:28'!
InnerTextMorph removeSelector: #showsBlinkingCursor!
!methodRemoval: EntryFieldMorph #showsBlinkingCursor stamp: 'jmv 9/10/2025 09:14:27'!
EntryFieldMorph removeSelector: #showsBlinkingCursor!

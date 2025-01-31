'From Cuis7.3 [latest update: #7041] on 31 January 2025 at 4:15:02 pm'!

!Morph methodsFor: 'event handling testing' stamp: 'jmv 1/31/2025 15:52:09'!
tabToFocus
	"Answer true if tab and shift+tab are used to transfer keyboard focus between windgets.
	See senders."
	^true! !


!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 1/31/2025 15:54:40'!
tabToFocus
	"Answer true if tab and shift+tab are used to transfer keyboard focus between windgets.
	See senders."
	^(self valueOfProperty: #tabIsValidInput) == false! !

!InnerTextMorph methodsFor: 'event handling testing' stamp: 'jmv 1/31/2025 15:54:12'!
tabToFocus: aBoolean
	"Enable or disable #tabToFocus.
	Disabled means that tab is valid character input."
	^self setProperty: #tabIsValidInput toValue: aBoolean! !


!KeyboardEvent methodsFor: 'testing' stamp: 'jmv 1/31/2025 16:11:51'!
isTab
	"On the Mac, ctrl-shift-tab has keyValue = 25"
	^keyValue = 9 or: [ keyValue = 25 ]! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 1/31/2025 16:14:15'!
focusKeyboardFor: aKeyboardEvent

	"For most widgets, tab / shift-tab control focus. For those that not, ctrl-tab / ctrl-shift-tab is an alternative."
	aKeyboardEvent isTab ifTrue:
		[
			(self tabToFocus or: [aKeyboardEvent controlKeyPressed]) ifTrue:
				[
					aKeyboardEvent shiftPressed
						ifFalse: [ aKeyboardEvent hand keyboardFocusNext ]
						ifTrue: [ aKeyboardEvent hand keyboardFocusPrevious ].
					^ true ]].

	"Cycle through windows with cmdAlt + < and cmdAlt + >.
	VM and platform peculiarities are hidden in #isCmdAltLessThan and #isCmdAltGreaterThan"
	"This was done as an attempt to mimic the Mac OSX keystrokes for 'Move focus to next window in active application'. Unfortunately, it only works if OS X is set to use any other keys for this. If (as for example, with German defaults), OS-X uses these keystrokes, then they are not sent to the VM. This is a long standing issues in Chromium and PhotoShop, for example..."
	self disableCode: [
		aKeyboardEvent isCmdAltLessThan ifTrue: [
			aKeyboardEvent hand activatePreviousWindow.
			^true ].
		aKeyboardEvent isCmdAltGreaterThan ifTrue: [
			aKeyboardEvent hand activateNextWindow.
			^true ]].
	"Alternative for Mac OS-X: option-Tab and option-shift-Tab"
	(aKeyboardEvent isTab and: [ aKeyboardEvent rawMacOptionKeyPressed ])
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifTrue: [ aKeyboardEvent hand activatePreviousWindow ]
				ifFalse: [ aKeyboardEvent hand activateNextWindow ].
			^ true ].
	"Alternative for non-Mac OS-X: alt-< and alt->"
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter == $< ]) ifTrue: [
		aKeyboardEvent hand activatePreviousWindow.
		^true ].
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter == $> ]) ifTrue: [
		aKeyboardEvent hand activateNextWindow.
		^true ].
	^false! !


!HandMorph methodsFor: 'focus handling' stamp: 'jmv 1/31/2025 15:57:23'!
keyboardFocusNext

	| m |
	"Right now, only do it within a Window.
	We could add a preference, though.
	See #keyboardFocusPrevious"
	true ifTrue: [
		^self keyboardFocusNextWithinWindow ].

	m := self nextFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: false.
		self newKeyboardFocus: m ]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 1/31/2025 15:57:19'!
keyboardFocusPrevious

	| m |
	"Right now, only do it within a Window.
	We could add a preference, though.
	See #keyboardFocusNext"
	true ifTrue: [
		^self keyboardFocusPreviousWithinWindow ].

	m := self previousFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: true.
		self newKeyboardFocus: m ]! !

!methodRemoval: EntryFieldMorph #focusKeyboardFor: stamp: 'jmv 1/31/2025 15:57:46'!
EntryFieldMorph removeSelector: #focusKeyboardFor:!

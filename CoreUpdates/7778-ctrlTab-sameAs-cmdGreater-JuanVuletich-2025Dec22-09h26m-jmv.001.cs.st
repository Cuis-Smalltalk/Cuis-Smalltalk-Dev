'From Cuis7.7 [latest update: #7777] on 22 December 2025 at 9:30:05 am'!

!KeyboardEvent methodsFor: 'platform peculiarities' stamp: 'jmv 22/Dec/2025 08:52:06'!
isCtrlTab

	^self controlKeyPressed and: [ self keyCharacter = Character tab ]! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 22/Dec/2025 09:28:12'!
focusKeyboardFor: aKeyboardEvent

	"Cycle through windows using ctrl-tab"
	aKeyboardEvent isCtrlTab ifTrue: [
		aKeyboardEvent hand activateNextWindow.
		^true ].

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


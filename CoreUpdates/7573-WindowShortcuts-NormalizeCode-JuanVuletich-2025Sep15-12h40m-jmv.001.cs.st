'From Cuis7.5 [latest update: #7570] on 15 September 2025 at 12:49:00 pm'!

!SystemWindow methodsFor: 'submorphs-menus' stamp: 'jmv 9/15/2025 12:42:52'!
menuNshortcutsSpecsDo: aBlock
	"Iterate over our own menu and shortcut specs.
	Specific implementation in SystemWindows."

	self windowMenuSpec do: aBlock.! !

!SystemWindow methodsFor: 'submorphs-menus' stamp: 'jmv 9/15/2025 12:48:12'!
windowMenuSpec
	"Spec for the menu and shortcut keys for SystemWindows.
	See coment at Morph >> #getMenu
	"
	^
	#(
		"Keyboard shortcut only. No menu entry."
		#(991 	nil 			$w 	window 	delete)
	)! !


!KeyboardEvent methodsFor: 'dispatching' stamp: 'jmv 9/15/2025 12:44:50'!
sendEventTo: aMorph
	"Event dispatch finished. Deliver us straight to aMorph for execution."
"
Revisar senders de
#definesKeyboardShortcuts.
"
	keyEventType == #keystroke ifTrue: [
		self isFindClassShortcut ifTrue: [ 
			(Preferences at: #classFinder) value.
			self wasHandled: true.
			^self ].
		aMorph processKeystroke: self.
		^self ].
	keyEventType == #keyDown ifTrue: [
		aMorph processKeyDown: self.
		^self ].
	keyEventType == #keyUp ifTrue: [ 
		aMorph processKeyUp: self.
		^self ].! !

!methodRemoval: KeyboardEvent #isCloseWindowShortcut stamp: 'jmv 9/15/2025 12:45:10'!
KeyboardEvent removeSelector: #isCloseWindowShortcut!
!methodRemoval: KeyboardEvent #closeCurrentWindowOf: stamp: 'jmv 9/15/2025 12:44:59'!
KeyboardEvent removeSelector: #closeCurrentWindowOf:!

!KeyboardEvent reorganize!
('comparing' = hash)
('dispatching' dispatchIn: sendEventTo: startDispatchFromHand:)
('keyboard' keyCharacter keyValue)
('testing' isAlphaNumeric isArrowDown isArrowLeft isArrowRight isArrowUp isBackspace isColon isCtrlSpace isDelete isEnd isEsc isFindClassShortcut isHome isKeyDown isKeyUp isKeyboard isKeystroke isPageDown isPageUp isQuestionMark isReturnKey isSpace isTab)
('private' setType:buttons:position:keyValue:hand:stamp:)
('platform peculiarities' isCmdAltGreaterThan isCmdAltLessThan)
!


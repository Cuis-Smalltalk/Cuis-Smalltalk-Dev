'From Cuis 6.0 [latest update: #5307] on 23 June 2022 at 4:11:12 pm'!

!Morph methodsFor: 'focus handling' stamp: 'jmv 6/23/2022 16:09:26'!
keyboardFocused
	"Just given focus via keyboard."! !


!PluggableListMorph methodsFor: 'event handling' stamp: 'jmv 6/23/2022 16:10:12'!
keyboardFocused
	"Just given focus via keyboard.
	Autoselect first row."

	(scroller selectedRow = 0 and: [
		self allowsMultipleSelection not and: [ self getListSize > 0 ]])
			ifTrue: [ self navigateToTop ].! !


!HandMorph methodsFor: 'focus handling' stamp: 'jmv 6/23/2022 15:48:54'!
keyboardFocusNextWithinWindow
	| window |
	window _ (keyboardFocus ifNil: [^ false]) owningWindow.
	(keyboardFocus previousMorphThat: [ :m |
		m visible and: [m handlesKeyboard and: [m owningWindow = window]]]) ifNotNil: [ :aMorph |
			self newKeyboardFocus: aMorph.
			aMorph keyboardFocused.
			^true ].
	^false! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 6/23/2022 15:49:06'!
keyboardFocusPreviousWithinWindow
	| window |
	window _ (keyboardFocus ifNil: [^ false]) owningWindow.
	(keyboardFocus nextMorphThat: [ :m |
		m visible and: [m handlesKeyboard and: [m owningWindow = window]]]) ifNotNil: [ :aMorph |
			self newKeyboardFocus: aMorph.
			aMorph keyboardFocused.
			^true ].
	^false! !


!PluggableListMorph methodsFor: 'event handling' stamp: 'jmv 6/23/2022 15:49:35'!
keyboardFocusChange: aBoolean
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw focus feedback"
	drawKeyboardFocusIndicator
		ifTrue: [self redrawNeeded]
		ifFalse: [
			scroller selectedRow
				ifNotNil: [ :i |
					scroller selectedRow: nil; selectedRow: i ]]! !


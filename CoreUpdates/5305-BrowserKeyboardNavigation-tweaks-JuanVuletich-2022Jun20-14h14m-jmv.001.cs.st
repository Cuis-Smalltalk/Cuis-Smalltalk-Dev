'From Cuis 6.0 [latest update: #5304] on 20 June 2022 at 2:30:09 pm'!

!PluggableListMorph methodsFor: 'testing' stamp: 'jmv 6/20/2022 14:23:19'!
allowsMultipleSelection
	^false! !


!PluggableListMorphOfMany methodsFor: 'testing' stamp: 'jmv 6/20/2022 14:23:12'!
allowsMultipleSelection
	^true! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 6/20/2022 14:27:00'!
focusKeyboardFor: aKeyboardEvent
	(super focusKeyboardFor: aKeyboardEvent)
		ifTrue: [^ true].
	aKeyboardEvent shiftPressed ifTrue: [
		aKeyboardEvent isArrowLeft
			ifTrue: [ ^ aKeyboardEvent hand keyboardFocusPreviousWithinWindow ].
		aKeyboardEvent isArrowRight
			ifTrue: [ ^ aKeyboardEvent hand keyboardFocusNextWithinWindow ]].
	^ false! !


!PluggableListMorph methodsFor: 'event handling' stamp: 'jmv 6/20/2022 14:24:53'!
keyboardFocusChange: aBoolean
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw focus feedback"

	(scroller selectedRow = 0 and: [
		self allowsMultipleSelection not and: [ self getListSize > 0 ]])
			ifTrue: [ self navigateToTop ].
	drawKeyboardFocusIndicator
		ifTrue: [self redrawNeeded]
		ifFalse: [
			scroller selectedRow
				ifNotNil: [ :i |
					scroller selectedRow: nil; selectedRow: i ]]! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 6/20/2022 14:27:46'!
focusKeyboardFor: aKeyboardEvent
	(super focusKeyboardFor: aKeyboardEvent)
		ifTrue: [^ true].
	aKeyboardEvent isArrowLeft
		ifTrue: [ ^ aKeyboardEvent hand keyboardFocusPreviousWithinWindow ].
	aKeyboardEvent isArrowRight
		ifTrue: [ ^ aKeyboardEvent hand keyboardFocusNextWithinWindow ].
	^ false! !

!methodRemoval: ChangeSorter #currentCngSet stamp: 'jmv 6/20/2022 14:28:33'!
ChangeSorter removeSelector: #currentCngSet!

!PluggableListMorphOfMany reorganize!
('drawing' listSelectionAt: listSelectionAt:put:)
('events' mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('initialization' model:listGetter:primarySelectionGetter:primarySelectionSetter:listSelectionGetter:listSelectionSetter:mainView:menuGetter:keystrokeAction:)
('updating' update:)
('model access' itemSelectedAmongMultiple:)
('testing' allowsMultipleSelection)
!


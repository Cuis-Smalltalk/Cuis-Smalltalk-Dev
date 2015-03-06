'From Cuis 4.2 of 25 July 2013 [latest update: #2204] on 6 March 2015 at 9:01:20.281835 am'!

!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 3/6/2015 08:54'!
disableEdition
	self setProperty: #disablesEdition toValue: true.
	self stopBlinking! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 3/6/2015 08:56'!
showsBlinkingCursor

	^self handlesKeyboard and: [ self disablesEdition not ]! !

!OneLineEditorMorph methodsFor: 'event handling testing' stamp: 'jmv 3/6/2015 08:53'!
disablesEdition

	^self hasProperty: #disablesEdition! !

!OneLineEditorMorph methodsFor: 'geometry' stamp: 'jmv 3/6/2015 08:52'!
minimumExtent

	^ self measureContents! !


!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 3/6/2015 08:59'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking _ true.
	self showsBlinkingCursor ifTrue: [
		"Show cursor right now if needed"
		showTextCursor ifFalse: [
			showTextCursor _ true.
			textCursorRect ifNotNil: [ :r | self invalidateLocalRect: r ]]]! !

!OneLineEditorMorph methodsFor: 'events-processing' stamp: 'jmv 3/6/2015 08:59'!
keyboardFocusChange: aBoolean 
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor ].	"Forces install"
			self selectAll.
			self showsBlinkingCursor ifTrue: [
				self startBlinking ]]
		ifFalse: [
			self stopBlinking.
			keyboardFocusWatcher ifNotNil: [
				keyboardFocusWatcher lostFocus: self ]].
	self redrawNeeded! !


!OneLineEditorMorph reorganize!
('accessing' baseFont contents contents: crAction crAction: disableEdition editor fitContents fontToUse keyboardFocusWatcher: measureContents)
('blink cursor' onBlinkCursor pauseBlinking showsBlinkingCursor startBlinking stopBlinking)
('drawing' characterIndexAtPoint: displayTextCursorAtX:top:bottom:emphasis:on: drawOn: drawSelectionOn: drawTextCursorOn:)
('editing' handleInteraction:)
('events' clickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseEnter: mouseMove:localPosition:)
('event handling testing' disablesEdition handlesKeyboard handlesMouseDown: handlesMouseOver:)
('events-processing' focusKeyboardFor: keyboardFocusChange:)
('geometry' minimumExtent)
('initialization' defaultColor initWithContents:font:emphasis: initialize)
('testing' hasSelection hasTextCursor)
('typing/selecting keys' selectAll)
('unaccepted edits' hasUnacceptedEdits:)
('private' installEditor updateFromContents)
!


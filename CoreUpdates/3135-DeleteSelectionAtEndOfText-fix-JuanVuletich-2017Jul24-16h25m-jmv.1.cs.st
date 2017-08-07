'From Cuis 5.0 of 7 November 2016 [latest update: #3129] on 24 July 2017 at 4:30:44 pm'!

!TextEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 7/24/2017 09:10:47'!
forwardDelete: aKeyboardEvent
	"Delete forward over the next character."
	"This is a user command, and generates undo"

	| startIndex stopIndex |

	"If there was a selection"
	self hasSelection ifTrue: [
		self replaceSelectionWith: self nullText.
		^ false].

	"Exit if at end"
	startIndex _ self markIndex.
	startIndex > model textSize ifTrue: [
		^ false].

	"Null selection - do the delete forward"
	stopIndex _ startIndex.
	(aKeyboardEvent keyValue = 127 and: [ aKeyboardEvent shiftPressed ])
		ifTrue: [stopIndex := (self nextWordStart: stopIndex) - 1].
	self selectFrom: startIndex to: stopIndex.
	self replaceSelectionWith: self nullText.
	self deselectAndPlaceCursorAt: startIndex.
	^false! !


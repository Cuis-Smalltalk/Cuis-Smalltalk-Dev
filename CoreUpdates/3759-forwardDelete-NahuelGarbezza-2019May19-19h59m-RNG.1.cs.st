'From Cuis 5.0 of 7 November 2016 [latest update: #3754] on 19 May 2019 at 8:10:56 pm'!
"Change Set:		3755-CuisCore-AuthorName-2019May19-19h59m
Date:			19 May 2019
Author:			Nahuel Garbezza

Delete words forward using Ctrl+Del on Win/Linux and Option-Del in Mac"!


!TextEditor methodsFor: 'private' stamp: 'RNG 5/19/2019 20:02:41'!
shouldDeleteAWordForward: aKeyboardEvent

	^ aKeyboardEvent isDelete and: [
		aKeyboardEvent rawMacOptionKeyPressed or: [
			aKeyboardEvent controlKeyPressed ] ]! !

!TextEditor methodsFor: 'private' stamp: 'RNG 5/19/2019 20:09:09'!
shouldHandleUsingCmdShortcuts: aKeyboardEvent 
	^ (aKeyboardEvent keyValue between: 32 and: 126) and: [ aKeyboardEvent commandAltKeyPressed ]! !


!KeyboardEvent methodsFor: 'testing' stamp: 'RNG 5/19/2019 20:01:06'!
isDelete

	^ keyValue = 127! !


!TextEditor methodsFor: 'typing support' stamp: 'RNG 5/19/2019 20:09:09'!
dispatchOn: aKeyboardEvent
	"Carry out the action associated with this character, if any."

	| asciiValue c |
	self clearParens.
  	asciiValue _ aKeyboardEvent keyValue.
	"Control keys are handled by #shortcuts even if they have any modifiers"
	(self shouldHandleUsingCmdShortcuts: aKeyboardEvent) ifTrue: [
		^self perform: (self cmdShortcuts at: asciiValue + 1) with: aKeyboardEvent ].

	c _ aKeyboardEvent keyCharacter.
	(')]}' includes: c)
		ifTrue: [ self blinkPrevParen: c ].

	^ self perform: (self shortcuts at: asciiValue + 1) with: aKeyboardEvent! !

!TextEditor methodsFor: 'typing/selecting keys' stamp: 'RNG 5/19/2019 20:02:24'!
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
	(self shouldDeleteAWordForward: aKeyboardEvent)
		ifTrue: [stopIndex := (self nextWordStart: stopIndex) - 1].
	self selectFrom: startIndex to: stopIndex.
	self replaceSelectionWith: self nullText.
	self deselectAndPlaceCursorAt: startIndex.
	^false! !


!TextEditor reorganize!
('accessing' actualContents actualContents: currentAttributes currentCharacterStyleOrNil currentParagraphStyle defaultFont defaultFont: lastFont lastParagraphStyleOrNil pointBlock replaceSelectionWith: selectionRectangle setSearch: text totalTextHeight visibleHeight)
('accessing-selection' characterBlockForIndex: hasSelection markIndex markIndex: markIndex:pointIndex: pointIndex pointIndex: selection selectionAsStream selectionIntervalsDo: startBlock startIndex stopBlock stopIndex)
('attributes' changeEmphasisOrAlignment clearFont offerColorMenu offerDefaultFontMenu offerFontMenu)
('current selection' recomputeSelection)
('displaying' flash)
('editing keys' align: changeEmphasis: changeLineEndsToLf: chooseColor compareToClipboard: copyHiddenInfo enclose: hiddenInfo inOutdent:delta: indent: makeCapitalized: makeLowercase: makeUppercase: offerColorMenu: offerFontMenu: outdent: tabKey: undo:)
('events' clickAndHalf doubleClickAndHalf mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('menu messages' acceptContents align chooseAlignment compareToClipboard copySelection cut find findAgain openHelp pasteRecent setSearchString wrapOnOff)
('model access' model: wordUnderCursor)
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: deselectAndPlaceCursorAt: insertAndSelect:at: lineSelectAndEmptyCheck: messageSendsRanges: selectFrom:to: selectInterval: selectLine)
('nonediting/nontyping keys' cursorEnd: cursorHome: help: setSearchString:)
('parenblinking' blinkParen blinkParenAt: blinkPrevParen: clearParens)
('scrolling')
('typing support' addString: backTo: dispatchOn: processKeyStroke: setEmphasisHereFromText setEmphasisHereFromTextForward:)
('typing/selecting keys' backWord: find: findAgain: forwardDelete: selectAll)
('private' addAttributesForPasting: applyAttribute: beginningOfLine: endOfLine: findAndReplaceMany: findAndReplaceOnce: firstOfBeginningOfLineOrEndOfIndentationLeftOf: indent:fromStream:toStream: isDisjointFrom: moveCursor:forward:event: nullText pageHeight privateCurrentString sameColumn:newLine:forward: shouldDeleteAWordForward: shouldHandleUsingCmdShortcuts: storeSelectionInComposition unapplyAttributes: zapMultiSelection)
('binding' bindingOf:)
('commands' insertMorph:at: removeMorph:)
('undo & redo' flushUndoRedoCommands offerUndoHistory redo redo: undo)
('menu' getMenu)
('initialization' initialize resetState textComposition:)
('testing' hasUnacceptedEdits hasUnacceptedEdits:)
!


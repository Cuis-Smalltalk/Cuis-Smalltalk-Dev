'From Cuis6.3 [latest update: #6203] on 4 February 2024 at 6:22:13 pm'!

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'bp 2/4/2024 18:16:16'!
scrollToShow: aRectangle
	"scroll to include as much of aRectangle as possible, where aRectangle is in the scroller's local space.
	This means that 0@0 is scrolling all the way top and all the way left"
	| deltaX deltaY |
	deltaX := (aRectangle amountToTranslateWithin: (`0@0` extent: self viewableExtent)) x.
	deltaY := (aRectangle amountToTranslateWithin: (`0@0` extent: self viewableExtent)) y.
	(deltaX ~= 0 or: [deltaY ~= 0]) ifTrue: [
		self scrollBy: deltaX@deltaY ]! !


!TextEditor reorganize!
('accessing' actualContents actualContents: currentAttributes currentCharacterStyleOrNil currentParagraphStyle defaultFont defaultFont: lastFont lastParagraphStyleOrNil replaceSelectionWith: selectionRectangle setSearch: text totalTextHeight userHasEdited visibleHeight)
('accessing-selection' characterBlockForIndex: hasSelection markIndex markIndex: markIndex:pointIndex: pointBlock pointIndex pointIndex: selectedString selection selectionIsEmptyOrSeparators startBlock startIndex stopBlock stopIndex)
('attributes' clearFont offerColorMenu offerFontMenu offerFontSizeMenu offerFontStyleMenu)
('current selection' recomputeSelection)
('displaying' flash)
('editing keys' align: changeEmphasis: changeLineEndsToLf: chooseColor compareToClipboard: copyHiddenInfo enclose: hiddenInfo inOutdent:delta: indent: makeCapitalized: makeLowercase: makeUppercase: offerColorMenu: offerFontMenu: outdent: save: tabKey: undo:)
('events' clickAndHalfAt: doubleClickAndHalf mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('menu messages' acceptContents align chooseAlignment compareToClipboard copySelection cut find findAgain openHelp pasteRecent setSearchString wrapOnOff)
('model access' model: wordAt: wordUnderCursor)
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: insertAndSelect:at: selectFrom:to: selectLine selectWordOrDelimitedTextAt:)
('nonediting/nontyping keys' cursorEnd: cursorHome: help: setSearchString:)
('parenblinking' blinkParen blinkParenAt: blinkPrevParen: clearParens)
('typing support' addString: backTo: dispatchOn: processKeystrokeEvent: setEmphasisHereFromText setEmphasisHereFromTextForward:)
('typing/selecting keys' backWord: find: findAgain: forwardDelete:)
('private' addAttributesForPasting: applyAttribute: beginningOfLine: endOfLine: findAndReplaceMany: findAndReplaceOnce: firstOfBeginningOfLineOrEndOfIndentationLeftOf: indent:fromStream:toStream: isDisjointFrom: moveCursor:forward:event: nullText pageHeight privateCurrentString sameColumn:newLine:forward: shouldDeleteAWordForward: shouldHandleUsingCmdShortcuts: storeSelectionInComposition unapplyAttributes:)
('binding' bindingOf:)
('commands' insertMorph:at: removeMorph:)
('undo & redo' flushUndoRedoCommands offerUndoHistory redo redo: undo)
('menu' addCutAndPasteMenuSectionTo: addFinderMenuSectionTo: addStyleMenuSectionTo: addUndoMenuSectionTo: getMenu)
('initialization' initialize resetState textComposition:)
('testing' hasUnacceptedEdits hasUnacceptedEdits:)
!


'From Cuis 5.0 of 7 November 2016 [latest update: #3634] on 1 March 2019 at 1:52:22 pm'!

!InnerTextMorph methodsFor: 'notifications' stamp: 'jmv 3/1/2019 00:34:30'!
fontPreferenceChanged

	super fontPreferenceChanged.
	hasUnacceptedEdits ifFalse: [
		model refetch ].
	self updateFromTextComposition.! !


!InnerTextMorph reorganize!
('accessing' askBeforeDiscardingEdits: contents: contentsAsIs: crAction crAction: disableEditing editor enableEditing escAction escAction: hasEditingConflicts hasEditingConflicts: isWrapped model: model:wrappedTo: textColor textColor: wrapFlag:)
('anchors' anchorMorph:at:)
('caching' releaseCachedState)
('drawing' debugDrawLineRectsOn: drawOn:)
('editing' acceptContents acceptOnCR chooseEmphasisOrAlignment chooseFont confirmAcceptAnyway enterClickableRegion:localPosition: handleInteraction: hasUnacceptedEdits:)
('events' clickAndHalf:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('event handling testing' disablesEditing handlesKeyboard handlesMouseDown:)
('event handling' keyboardFocusChange: processKeyStroke:)
('events-processing' processKeystroke:localPosition: processMouseMove:localPosition:)
('geometry' adjustExtent minimumExtent privateExtent:)
('initialization' defaultColor initialize)
('menu' addCustomMenuItems:hand: getMenu wrapOnOff wrapString)
('submorphs-add/remove' addMorphFrontFromWorldPosition:)
('private' autoCompleterClass: extentForComposing fit installEditorAndTextComposition mouseButton2Activity mutex releaseEditorAndTextComposition removedMorph: resetTextComposition selectionChanged stylerClass: textComposition updateFromTextComposition)
('notifications' fontPreferenceChanged possiblyChanged someSubmorphPositionOrExtentChanged)
('testing' canDiscardEdits hasUnacceptedEdits)
('blinking cursor' onBlinkCursor pauseBlinking showsBlinkingCursor startBlinking stopBlinking)
('accept/cancel' acceptOnCR:)
('macpal' flash)
('miscellaneous' disregardUnacceptedEdits selectAll)
('shout' formatAndStyleIfNeeded stylerStyled)
('selection' scrollSelectionIntoView)
('classification' is:)
!


'From Cuis 4.2 of 25 July 2013 [latest update: #2378] on 21 June 2015 at 2:14:46.552028 pm'!

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 6/21/2015 14:11'!
autoCompleterClass: aTextCompleterClass
	autoCompleter class == aTextCompleterClass ifFalse: [
		autoCompleter _ aTextCompleterClass ifNotNil: [
			aTextCompleterClass withModel: model ].
		autoCompleter ifNotNil: [
			autoCompleter textMorph: self ]]! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 6/21/2015 14:12'!
stylerClass: aTextStylerClass
	styler class == aTextStylerClass ifFalse: [
		styler _ aTextStylerClass ifNotNil: [
			aTextStylerClass new ].
		styler ifNotNil: [
			model ifNotNil: [
				styler textModel: model ]]]! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 6/21/2015 14:13'!
installEditorAndTextComposition
	"Install an editor for my textComposition. Install also the textComposition."
	| e tc |
	
	"Editor and TextComposition are assigned here atomically."
	e _ model editorClass new morph: self.
	e model: model.
	tc _ TextComposition new.
	"Keep critical section short"
	self mutex critical: [
		editor _ e.
		textComposition _ tc ].
	tc
		setModel: model;
		extentForComposing: self extentForComposing.
	e textComposition: tc.
	tc editor: e.
	e setEmphasisHereFromText.
	tc composeAll.
	e resetState.
	self fit.
	self selectionChanged.

	"Add extras. Text Styler and Autocompleter"
	self stylerClass:
		(Preferences syntaxHighlightingAsYouType ifTrue: [
			model textStylerClass ]).
	self autoCompleterClass:
		model autoCompleterClass! !

!methodRemoval: InnerTextMorph #autoCompleter:!
InnerTextMorph removeSelector: #autoCompleter:!
!methodRemoval: InnerTextMorph #styler:!
InnerTextMorph removeSelector: #styler:!

!InnerTextMorph reorganize!
('accessing' askBeforeDiscardingEdits: contents: contentsAsIs: crAction disableEdition editor hasEditingConflicts hasEditingConflicts: isWrapped model: model:wrappedTo: textColor textColor: wrapFlag:)
('anchors' anchorMorph:at:)
('caching' releaseCachedState)
('drawing' debugDrawLineRectsOn: drawOn:)
('editing' acceptContents acceptOnCR cancelEdits chooseEmphasisOrAlignment chooseFont enterClickableRegion:localPosition: handleInteraction: hasUnacceptedEdits:)
('events' clickAndHalf:localPosition: doubleClickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('event handling testing' disablesEdition handlesKeyboard handlesMouseDown:)
('event handling' keyboardFocusChange: processKeyStroke:)
('events-processing' processKeystroke:localPosition: processMouseMove:localPosition:)
('geometry' adjustExtent minimumExtent privateExtent:)
('initialization' defaultColor initialize)
('menu' addCustomMenuItems:hand: getMenu wrapOnOff wrapString)
('submorphs-add/remove' addMorphFrontFromWorldPosition:)
('private' autoCompleterClass: extentForComposing fit installEditorAndTextComposition mouseButton2Activity mutex releaseEditorAndTextComposition removedMorph: resetTextComposition selectionChanged stylerClass: textComposition updateFromTextComposition)
('notifications' possiblyChanged)
('testing' canDiscardEdits hasUnacceptedEdits)
('blinking cursor' onBlinkCursor pauseBlinking showsBlinkingCursor startBlinking stopBlinking)
('accept/cancel' acceptOnCR:)
('macpal' flash)
('miscellaneous' disregardUnacceptedEdits selectAll)
('shout' formatAndStyleIfNeeded okToStyle stylerStyled)
('selection' scrollSelectionIntoView)
('classification' is:)
!


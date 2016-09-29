'From Cuis 4.2 of 25 July 2013 [latest update: #2940] on 19 September 2016 at 10:02:09 am'!

!TextEditor methodsFor: 'initialization' stamp: 'jmv 9/19/2016 10:01:02'!
initialize
	selectionStartBlocks _ #().
	selectionStopBlocks _ #().
	initialSelectionStart _ nil.
	initialSelectionStop _ nil.
	doWordSelection _ false.
	doParagraphSelection _ false.
	defaultFont _ AbstractFont default! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 9/19/2016 10:01:10'!
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


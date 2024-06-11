'From Cuis7.1 [latest update: #6463] on 11 June 2024 at 6:08:49 pm'!

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 6/11/2024 18:07:08'!
installEditorAndTextComposition
	"Install an editor for my textComposition. Install also the textComposition."
	| e tc |
	
	"Editor and TextComposition are assigned here atomically."
	e := model editorClass new morph: self.
	e model: model.
	tc := TextComposition new.
	"Keep critical section short"
	self mutex critical: [
		editor := e.
		textComposition := tc.
		tc
			setModel: model;
			extentForComposing: self extentForComposing.
		e textComposition: tc.
		tc editor: e ].
	e setEmphasisHereFromText.
	tc composeAll.
	e resetState.
	self fit.
	self selectionChanged.
	self possiblyChanged.

	"Add extras. Text Styler and Autocompleter"
	self stylerClass:
		((Preferences at: #syntaxHighlightingAsYouType) ifTrue: [
			model textStylerClass ]).
	self autoCompleterClass:
		model autoCompleterClass! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 6/11/2024 18:07:12'!
releaseEditorAndTextComposition
	"Editor and TextComposition instantiation is lazy -- they will be created only when needed"

	editor := nil.
	textComposition := nil.
	self possiblyChanged.! !


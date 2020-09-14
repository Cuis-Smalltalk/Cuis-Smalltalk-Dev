'From Cuis 5.0 [latest update: #4375] on 14 September 2020 at 11:36:44 am'!

!TextEditor methodsFor: 'private' stamp: 'jmv 9/14/2020 11:34:18'!
storeSelectionInComposition
	"for proper display of selected text"

	pointBlock ifNil: [ ^self ].
	textComposition
		selectionStartBlocks: (selectionStartBlocks copyWith: self startBlock)
		selectionStopBlocks: (selectionStopBlocks copyWith: self stopBlock)! !

!TextEditor methodsFor: 'initialization' stamp: 'jmv 9/14/2020 11:35:52'!
resetState
	"Establish the initial conditions for editing the paragraph: place text cursor 
	before first character and set the emphasis to that of the first character"

	markBlock _ textComposition defaultCharacterBlock.
	pointBlock _ markBlock.
	selectionStartBlocks _ #().
	selectionStopBlocks _ #()! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 9/14/2020 11:27:57'!
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
		textComposition _ tc.
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

	"Add extras. Text Styler and Autocompleter"
	self stylerClass:
		(Preferences syntaxHighlightingAsYouType ifTrue: [
			model textStylerClass ]).
	self autoCompleterClass:
		model autoCompleterClass! !


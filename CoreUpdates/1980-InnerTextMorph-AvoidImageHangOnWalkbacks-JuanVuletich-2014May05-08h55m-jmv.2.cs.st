'From Cuis 4.2 of 25 July 2013 [latest update: #1979] on 5 May 2014 at 8:56:43.357 am'!

!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 5/5/2014 07:03'!
editor
	"Return my current editor, or install a new one."
	editor ifNil: [ self installEditorAndTextComposition ].
	^editor! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 5/5/2014 08:55'!
installEditorAndTextComposition
	"Install an editor for my textComposition. Install also the textComposition."
	| e tc |
	e _ model editorClass new morph: self.
	e model: model.

	"...Code here to recreate the TextComposition... We positively know it is nil."
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
	self selectionChanged! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 5/5/2014 07:03'!
textComposition
	"textComposition instantiation is lazy -- create it only when needed"
	textComposition ifNil: [ self installEditorAndTextComposition ].
	^textComposition! !


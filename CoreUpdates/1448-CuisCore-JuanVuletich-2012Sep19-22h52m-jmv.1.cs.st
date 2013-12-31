'From Cuis 4.0 of 21 April 2012 [latest update: #1445] on 19 September 2012 at 11:06:32 pm'!
!classDefinition: #InnerTextMorph category: #'Morphic-Views for Models'!
InnerPluggableMorph subclass: #InnerTextMorph
	instanceVariableNames: 'model wrapFlag paragraph editor pauseBlinking acceptOnCR hasUnacceptedEdits hasEditingConflicts askBeforeDiscardingEdits styler autoCompleter mutex '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 9/19/2012 23:05'!
mutex
	mutex
		ifNil: [ mutex := Mutex new ].
	^mutex! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 9/19/2012 23:06'!
editor
	"Return my current editor, or install a new one."
	self mutex critical: [
		editor ifNil: [ self installEditorAndParagraph ]].
	^editor! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 9/19/2012 22:54'!
installEditorAndParagraph
	"Install an editor for my paragraph. Install also the paragraph."
	editor _ model editorClass new morph: self.
	editor model: model.

	"...Code here to recreate the paragraph... We positively know it is nil."
	paragraph _ Paragraph new.
	paragraph
		setModel: model;
		extentForComposing: self extentForComposing.
	editor paragraph: paragraph.
	paragraph editor: editor.
	editor setEmphasisHereFromText.
	paragraph composeAll.
	self fit.
	editor resetState.
	self selectionChanged! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 9/19/2012 23:06'!
paragraph
	"Paragraph instantiation is lazy -- create it only when needed"
	self mutex critical: [
		paragraph ifNil: [ self installEditorAndParagraph ]].
	^paragraph! !

!classDefinition: #InnerTextMorph category: #'Morphic-Views for Models'!
InnerPluggableMorph subclass: #InnerTextMorph
	instanceVariableNames: 'model wrapFlag paragraph editor pauseBlinking acceptOnCR hasUnacceptedEdits hasEditingConflicts askBeforeDiscardingEdits styler autoCompleter mutex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

'From Cuis 4.1 of 12 December 2012 [latest update: #1539] on 4 January 2013 at 8:41:33 pm'!

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 1/4/2013 12:53'!
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
	editor resetState.
	self fit.
	self selectionChanged! !


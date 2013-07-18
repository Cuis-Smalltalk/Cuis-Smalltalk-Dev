'From Cuis 4.1 of 12 December 2012 [latest update: #1590] on 13 February 2013 at 12:14:23 am'!

!TextModelMorph methodsFor: 'updating' stamp: 'jmv 2/8/2013 13:48'!
update: aSymbol
	super update: aSymbol.
	aSymbol ifNil: [^self].
	aSymbol == #flash ifTrue: [^self flash].
	aSymbol == #actualContents ifTrue: [
		"Some day, it would be nice to keep objects and update them
		instead of throwing them away all the time for no good reason..."
		self textMorph releaseEditorAndParagraph.
		self textMorph formatAndStyleIfNeeded.
		self redrawNeeded.
		^self ].
	aSymbol == #acceptedContents ifTrue: [
		self textMorph hasUnacceptedEdits ifTrue: [
			self textMorph hasEditingConflicts: true.
			^self redrawNeeded ].
		model refetch.
		self setScrollDeltas.
		^self redrawNeeded ].
	aSymbol == #refetched ifTrue: [
		self setSelection: model getSelection.
		self hasUnacceptedEdits: false.
		^self ].
	aSymbol == #initialSelection ifTrue: [
		^self setSelection: model getSelection; redrawNeeded ].
	aSymbol == #autoSelect ifTrue: [
		TextEditor abandonChangeText.	"no replacement!!"
		self editor
			setSearch: model autoSelectString;
			findAndReplaceMany: true.
		self textMorph updateFromParagraph.
		^self scrollSelectionIntoView ].
	"Quite ugly"
	aSymbol == #clearUserEdits ifTrue: [
		^self hasUnacceptedEdits: false].
	aSymbol == #shoutStyle ifTrue: [
		self textMorph stylerStyled.
		^self redrawNeeded ]! !


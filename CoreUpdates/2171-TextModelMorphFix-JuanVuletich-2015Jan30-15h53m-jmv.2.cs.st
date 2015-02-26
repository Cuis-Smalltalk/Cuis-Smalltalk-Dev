'From Cuis 4.2 of 25 July 2013 [latest update: #2170] on 30 January 2015 at 3:54:08.348348 pm'!

!TextModelMorph methodsFor: 'updating' stamp: 'jmv 1/30/2015 15:52'!
update: aSymbol
	super update: aSymbol.
	aSymbol ifNil: [^self].
	aSymbol == #flash ifTrue: [^self flash].
	aSymbol == #actualContents ifTrue: [
		"Some day, it would be nice to keep objects and update them
		instead of throwing them away all the time for no good reason..."
		self textMorph
			releaseEditorAndTextComposition;
			textComposition;
			formatAndStyleIfNeeded.
		self setScrollDeltas.
		self redrawNeeded.
		^self ].
	aSymbol == #acceptedContents ifTrue: [
		self textMorph hasUnacceptedEdits ifTrue: [
			self textMorph hasEditingConflicts: true.
			^self redrawNeeded ].
		model refetch.
		"#actualContents also signalled in #refetch. No need to repeat what's done there."
		^self ].
	aSymbol == #refetched ifTrue: [
		"#actualContents also signalled when #refetched is signalled.
		No need to repeat what's done there."
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
		self textMorph updateFromTextComposition.
		^self scrollSelectionIntoView ].
	"Quite ugly"
	aSymbol == #clearUserEdits ifTrue: [
		^self hasUnacceptedEdits: false].
	aSymbol == #shoutStyle ifTrue: [
		self textMorph stylerStyled.
		^self redrawNeeded ]! !


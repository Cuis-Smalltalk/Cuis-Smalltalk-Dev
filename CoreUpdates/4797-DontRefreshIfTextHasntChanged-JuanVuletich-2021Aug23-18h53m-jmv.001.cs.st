'From Cuis 5.0 [latest update: #4796] on 23 August 2021 at 6:55:39 pm'!
!classDefinition: #InnerTextMorph category: 'Morphic-Widgets'!
InnerPluggableMorph subclass: #InnerTextMorph
	instanceVariableNames: 'model wrapFlag textComposition editor pauseBlinking acceptOnCR hasUnacceptedEdits hasEditingConflicts askBeforeDiscardingEdits styler autoCompleter mutex needsFit '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 8/23/2021 16:47:31'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	aBoolean == hasUnacceptedEdits ifFalse: [
		hasUnacceptedEdits _ aBoolean].
	aBoolean ifFalse: [ hasEditingConflicts _ false].

	"shout:  re-style the text iff aBoolean is true
	Do not apply any formatting (i.e. changes to the characters in the text),
	just styling (i.e. TextAttributes)"
	aBoolean ifTrue: [
		self formatAndStyleIfNeeded ].
	needsFit _ aBoolean.! !

!InnerTextMorph methodsFor: 'initialization' stamp: 'jmv 8/23/2021 16:47:24'!
initialize
	super initialize.
	wrapFlag _ true.
	acceptOnCR _ false.
	hasUnacceptedEdits _ false.
	hasEditingConflicts _ false.
	askBeforeDiscardingEdits _ true.
	needsFit _ false.! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 8/23/2021 18:54:55'!
updateFromTextComposition
	"A change has taken place in my textComposition, as a result of editing and I must be updated. "

	textComposition ifNotNil: [
		editor storeSelectionInComposition.
		needsFit ifTrue: [
			self fit.
			needsFit _ false ]].

	owner
		updateScrollBarsBounds;
		setScrollDeltas.! !

!classDefinition: #InnerTextMorph category: 'Morphic-Widgets'!
InnerPluggableMorph subclass: #InnerTextMorph
	instanceVariableNames: 'model wrapFlag textComposition editor pauseBlinking acceptOnCR hasUnacceptedEdits hasEditingConflicts askBeforeDiscardingEdits styler autoCompleter mutex needsFit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
InnerTextMorph allInstancesDo: [ :it | it instVarNamed: 'needsFit' put: false ].!


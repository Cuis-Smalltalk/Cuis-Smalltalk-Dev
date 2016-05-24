'From Cuis 4.2 of 25 July 2013 [latest update: #2766] on 18 May 2016 at 12:40:28.910145 pm'!

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 5/18/2016 12:39'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	aBoolean == hasUnacceptedEdits ifFalse: [
		hasUnacceptedEdits _ aBoolean.
		owner redrawNeeded].
	aBoolean ifFalse: [ hasEditingConflicts _ false].

	"shout:  re-style the text iff aBoolean is true
	Do not apply any formatting (i.e. changes to the characters in the text),
	just styling (i.e. TextAttributes)"
	(aBoolean and: [self okToStyle])
		ifTrue: [ styler formatAndStyle ]! !


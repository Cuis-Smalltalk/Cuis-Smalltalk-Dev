'From Cuis7.9 [latest update: #8313] on 5 November 2026 at 11:07:06 am'!

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 5/Nov/2026, 2:06:50 pm (UTC)'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	
	aBoolean ifTrue: [
		self convertAndStyleIfNeeded ].
	needsFit := aBoolean.
	
	aBoolean == hasUnacceptedEdits ifFalse: [
		hasUnacceptedEdits := aBoolean].
	aBoolean ifFalse: [ hasEditingConflicts := false].! !


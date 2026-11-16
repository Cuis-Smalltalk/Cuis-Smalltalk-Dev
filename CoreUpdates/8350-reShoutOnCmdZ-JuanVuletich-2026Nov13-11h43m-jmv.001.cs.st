'From Cuis7.9 [latest update: #8349] on 13 November 2026 at 11:45:14 am'!

!EntryFieldMorph methodsFor: 'unaccepted edits' stamp: 'jmv 13/Nov/2026, 2:40:24 pm (UTC)'!
userHasEdited! !


!TextModelMorph methodsFor: 'unaccepted edits' stamp: 'jmv 12/Nov/2026, 8:50:19 pm (UTC)'!
userHasEdited
	scroller userHasEdited.! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 12/Nov/2026, 8:50:09 pm (UTC)'!
userHasEdited
	
	self convertAndStyleIfNeeded.! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 12/Nov/2026, 8:51:01 pm (UTC)'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "

	needsFit := aBoolean.
	
	aBoolean == hasUnacceptedEdits ifFalse: [
		hasUnacceptedEdits := aBoolean].
	aBoolean ifFalse: [ hasEditingConflicts := false].! !


!Editor methodsFor: 'accessing' stamp: 'jmv 13/Nov/2026, 2:39:37 pm (UTC)'!
userHasEdited
	"Note that my text is not free of user edits."

	morph userHasEdited.
	morph hasUnacceptedEdits: true! !


!TextEditor methodsFor: 'accessing' stamp: 'jmv 12/Nov/2026, 8:51:25 pm (UTC)'!
userHasEdited

	morph userHasEdited.
	morph hasUnacceptedEdits: model isDirty.! !


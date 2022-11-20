'From Cuis 6.0 [latest update: #5510] on 12 November 2022 at 8:00:35 pm'!

!TextModel methodsFor: 'testing' stamp: 'SSC 11/12/2022 17:59:53'!
isDirty

	^undoRedoCommands position isZero not.! !


!PluggableTextModel methodsFor: 'testing' stamp: 'SSC 11/12/2022 19:25:43'!
isDirty

	textGetter ifNil: [ ^super isDirty ].
	^((textProvider perform: textGetter) = (CodeProvider normalizeCodeForCodeFiles: actualContents)) not.! !


!TextEditor methodsFor: 'accessing' stamp: 'SSC 11/12/2022 18:04:57'!
userHasEdited

	morph hasUnacceptedEdits: model isDirty! !


!InnerTextMorph methodsFor: 'editing' stamp: 'SSC 11/12/2022 18:08:00'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	
	(aBoolean or: hasUnacceptedEdits) ifTrue: [
		self formatAndStyleIfNeeded ].
	needsFit _ aBoolean.
	
	aBoolean == hasUnacceptedEdits ifFalse: [
		hasUnacceptedEdits _ aBoolean].
	aBoolean ifFalse: [ hasEditingConflicts _ false].
! !


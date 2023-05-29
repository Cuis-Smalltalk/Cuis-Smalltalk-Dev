'From Cuis 6.0 [latest update: #5820] on 29 May 2023 at 12:36:35 pm'!

!Morph methodsFor: 'testing' stamp: 'jmv 5/29/2023 12:31:30'!
disregardUnacceptedEdits
	"Return true if this view either has no text changes or does not care."

	submorphs do: [ :m | m disregardUnacceptedEdits ].! !


!InnerTextMorph methodsFor: 'miscellaneous' stamp: 'jmv 5/29/2023 12:33:57'!
disregardUnacceptedEdits

	needsFit :=​ false.
	hasUnacceptedEdits :=​ false.
	hasEditingConflicts :=​ false.! !


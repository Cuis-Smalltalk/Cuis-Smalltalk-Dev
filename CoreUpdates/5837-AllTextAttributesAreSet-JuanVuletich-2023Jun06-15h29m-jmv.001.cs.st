'From Cuis 6.0 [latest update: #5836] on 6 June 2023 at 3:30:15 pm'!

!Text class methodsFor: 'private' stamp: 'jmv 6/6/2023 15:29:47'!
addAttribute: newTextAttribute toArray: existingAttributes
	"Add a new text attribute to an existing set"

	| includeNew |
	includeNew := true.
	newTextAttribute reset.
	^ Array streamContents: [ :strm |
		existingAttributes do: [ :existing |
			(newTextAttribute dominates: existing) ifFalse: [
				"Keep existing unless new dominates it."
				strm nextPut: existing.
				"If new doesn't dominate existing, perhaps existing dominates new."
				(existing dominates: newTextAttribute) ifTrue: [
					"Don't add new if an existing attribute dominates it."
					includeNew := false ]
				]].
		includeNew ifTrue: [ strm nextPut: newTextAttribute ]].! !

!methodRemoval: TextEmphasis #isSet stamp: 'jmv 6/6/2023 15:29:52'!
TextEmphasis removeSelector: #isSet!
!methodRemoval: TextColor #isSet stamp: 'jmv 6/6/2023 15:29:54'!
TextColor removeSelector: #isSet!
!methodRemoval: TextBackgroundColor #isSet stamp: 'jmv 6/6/2023 15:29:53'!
TextBackgroundColor removeSelector: #isSet!
!methodRemoval: TextAttribute #isSet stamp: 'jmv 6/6/2023 15:29:53'!
TextAttribute removeSelector: #isSet!

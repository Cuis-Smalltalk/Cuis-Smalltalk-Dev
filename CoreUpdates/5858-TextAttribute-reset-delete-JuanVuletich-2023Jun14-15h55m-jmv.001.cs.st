'From Cuis 6.0 [latest update: #5857] on 14 June 2023 at 3:58:21 pm'!

!Text class methodsFor: 'private' stamp: 'jmv 6/14/2023 15:55:51'!
addAttribute: newTextAttribute toArray: existingAttributes
	"Add a new text attribute to an existing set"

	| includeNew |
	includeNew := true.
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

!methodRemoval: TextAttribute #reset stamp: 'jmv 6/14/2023 15:56:45'!
TextAttribute removeSelector: #reset!

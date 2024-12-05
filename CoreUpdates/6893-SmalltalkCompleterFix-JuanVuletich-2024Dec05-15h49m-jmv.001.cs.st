'From Cuis7.1 [latest update: #6891] on 5 December 2024 at 3:53:38 pm'!

!SmalltalkCompleter methodsFor: 'entries' stamp: 'jmv 12/5/2024 15:53:19'!
computeEntries

	| allSource source contextClass specificModel separator fragmentStart fragmentEnd done |

	specificModel := self textProviderOrModel.
	contextClass := self selectedClassOrMetaClassIn: specificModel.
	contextClass ifNil: [ ^entries := #() ].

	allSource := self allSource.
	(specificModel is: #CodeProvider)
		ifTrue: [source := allSource copyFrom: 1 to: position]
		ifFalse: [
			separator := String newLineString, String newLineString.
			fragmentStart := 1.
			done := false.
			[done] whileFalse: [
				fragmentEnd := allSource
					indexOfSubCollection: separator
					startingAt: fragmentStart
					ifAbsent: [allSource size].
				fragmentEnd >= position ifTrue: [
					source := allSource copyFrom: fragmentStart to: position.
					done := true ].
				fragmentStart := fragmentEnd+separator size ]].

	allRanges := self parse: source in: contextClass and: specificModel.
	currentRange := allRanges lastIfEmpty: [ ^entries := #() ].
	possibleInvalidSelectors := #().

	currentRange end = source size
		ifTrue: [ self computeEntriesOfMessageOrIdentifiersFor: source in: contextClass and: specificModel ]
	 	ifFalse: [ self computeMessageEntriesWithEmptyPrefixFor: source in: contextClass and: specificModel ].

	! !


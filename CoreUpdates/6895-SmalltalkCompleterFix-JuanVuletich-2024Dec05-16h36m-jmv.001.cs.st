'From Cuis7.1 [latest update: #6894] on 5 December 2024 at 4:38:02 pm'!

!MessageNames methodsFor: 'user interface support' stamp: 'jmv 12/5/2024 16:37:35'!
autoCompleterClassFor: textGetter
	^ (textGetter = #acceptedContents) ifTrue:
		[
			super autoCompleterClassFor: textGetter ].
! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'HAW 11/7/2023 10:53:53'!
computeEntries

	| allSource source contextClass specificModel separator fragmentStart fragmentEnd done |

	specificModel := self textProviderOrModel.
	contextClass := self selectedClassOrMetaClassIn: specificModel.

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


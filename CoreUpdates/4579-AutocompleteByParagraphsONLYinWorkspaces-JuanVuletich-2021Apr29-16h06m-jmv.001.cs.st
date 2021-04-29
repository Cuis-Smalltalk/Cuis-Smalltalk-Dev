'From Cuis 5.0 [latest update: #4574] on 29 April 2021 at 4:13:04 pm'!

!SmalltalkCompleter methodsFor: 'entries' stamp: 'jmv 4/29/2021 16:12:50'!
computeEntries

	| allSource source contextClass specificModel range
	separator fragmentStart fragmentEnd done |

	specificModel _ self textProviderOrModel.
	contextClass _ self selectedClassOrMetaClassIn: specificModel.

	allSource _ self allSource.
	(specificModel is: #CodeProvider)
		ifTrue: [source _ allSource copyFrom: 1 to: position]
		ifFalse: [
			separator _ String newLineString, String newLineString.
			fragmentStart _ 1.
			done _ false.
			[done] whileFalse: [
				fragmentEnd _ allSource
					indexOfSubCollection: separator
					startingAt: fragmentStart
					ifAbsent: [allSource size].
				fragmentEnd >= position ifTrue: [
					source _ allSource copyFrom: fragmentStart to: position.
					done _ true ].
				fragmentStart _ fragmentEnd+separator size ]].

	allRanges _ self parse: source in: contextClass and: specificModel.
	range _ allRanges lastIfEmpty: [ ^entries _ #() ].
	possibleInvalidSelectors _ #().

	range end = source size
		ifTrue: [ self computeEntriesOfMessageOrIdentifiersFor: source at: range in: contextClass and: specificModel ]
	 	ifFalse: [ self computeMessageEntriesWithEmptyPrefixFor: source at: range in: contextClass and: specificModel ].

	! !


'From Cuis 4.2 of 25 July 2013 [latest update: #2832] on 24 June 2016 at 9:53:00.971602 am'!

!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 6/24/2016 09:51'!
selection
	"Answer the text that is currently selected."

	| t firstIndex lastIndex |
	t _ model actualContents.
	firstIndex _ self startIndex.
	lastIndex _ self stopIndex - 1.
	(firstIndex = 1 and: [ lastIndex = t size ])
		ifTrue: [ ^t copy ].
	^Text streamContents: [ :strm |
		"Multiple selection"
		selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock | | toAppend |
			toAppend _ t copyFrom: startBlock stringIndex to: stopBlock stringIndex - 1.
			toAppend size > 0 ifTrue: [
				strm nextPutAll: toAppend.
				strm withAttributes: (toAppend attributesAt: toAppend size) do: [ strm newLine ]]].
		"Regular selection"
		strm nextPutAll: ( t copyFrom: firstIndex to: lastIndex ) ]! !


!SmalltalkEditor methodsFor: 'accessing-selection' stamp: 'jmv 6/24/2016 09:51'!
selection
	"Answer the text that is currently selected.
	Redefined for Smalltalk code: if there's no regular selection, and all the selectionBlocks contain the same string,
	answer that string."
	| t regularSelection allPartsEqual samePart firstIndex |
	t _ model actualContents.
	firstIndex _ self startIndex.
	lastIndex _ self stopIndex - 1.

	(firstIndex = 1 and: [ lastIndex = t size ])
		ifTrue: [ ^t copy ].

	regularSelection _ ( t copyFrom: firstIndex to: lastIndex).
	allPartsEqual _ true.
	samePart _ nil.
	^Text streamContents: [ :strm |
		"Multiple selection"
		selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock | | toAppend |
			toAppend _ t copyFrom: startBlock stringIndex to: stopBlock stringIndex - 1.
			toAppend size > 0 ifTrue: [
				samePart
					ifNil: [ samePart _ toAppend ]
					ifNotNil: [
						allPartsEqual _ allPartsEqual and: [ samePart = toAppend ]].
				strm nextPutAll: toAppend.
				strm withAttributes: (toAppend attributesAt: toAppend size) do: [ strm newLine ]].
			].
		(allPartsEqual and: [ regularSelection isEmpty ]) ifTrue: [
			^samePart ifNil: [ '' asText ]].
		"Regular selection"
		strm nextPutAll: regularSelection ]! !


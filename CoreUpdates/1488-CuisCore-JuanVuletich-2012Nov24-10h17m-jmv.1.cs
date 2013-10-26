'From Cuis 4.0 of 21 April 2012 [latest update: #1486] on 24 November 2012 at 10:19 am'!

!SmalltalkEditor methodsFor: 'accessing-selection' stamp: 'jmv 11/24/2012 10:18'!
selection
	"Answer the text that is currently selected.
	Redefined for Smalltalk code: if there's no regular selection, and all the selectionBlocks contain the same string,
	answer that string."
	| t regularSelection allPartsEqual samePart |
	t _ model actualContents.
	regularSelection _ ( t copyFrom: self startIndex to: self stopIndex - 1 ).
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


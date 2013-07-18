'From Cuis 4.1 of 12 December 2012 [latest update: #1583] on 31 January 2013 at 9:10:22 pm'!
!classDefinition: #SmalltalkEditor category: #'System-Text'!
TextEditor subclass: #SmalltalkEditor
	instanceVariableNames: 'lastIndex '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 1/31/2013 21:09'!
selection

	| firstIndex lastIndex |
	firstIndex _ self startIndex.
	lastIndex _ self stopIndex - 1.

	(firstIndex = 1 and: [ lastIndex = string size ])
		ifTrue: [ ^string ].

	^string copyFrom: firstIndex to: lastIndex! !


!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 1/31/2013 21:08'!
selection
	"Answer the text that is currently selected."

	| t firstIndex lastIndex |
	t _ model actualContents.
	firstIndex _ self startIndex.
	lastIndex _ self stopIndex - 1.
	(firstIndex = 1 and: [ lastIndex = t size ])
		ifTrue: [ ^t ].
	^Text streamContents: [ :strm |
		"Multiple selection"
		selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock | | toAppend |
			toAppend _ t copyFrom: startBlock stringIndex to: stopBlock stringIndex - 1.
			toAppend size > 0 ifTrue: [
				strm nextPutAll: toAppend.
				strm withAttributes: (toAppend attributesAt: toAppend size) do: [ strm newLine ]]].
		"Regular selection"
		strm nextPutAll: ( t copyFrom: firstIndex to: lastIndex ) ]! !


!SmalltalkEditor methodsFor: 'accessing-selection' stamp: 'jmv 1/31/2013 21:08'!
selection
	"Answer the text that is currently selected.
	Redefined for Smalltalk code: if there's no regular selection, and all the selectionBlocks contain the same string,
	answer that string."
	| t regularSelection allPartsEqual samePart firstIndex |
	t _ model actualContents.
	firstIndex _ self startIndex.
	lastIndex _ self stopIndex - 1.

	(firstIndex = 1 and: [ lastIndex = t size ])
		ifTrue: [ ^t ].

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

!classDefinition: #SmalltalkEditor category: #'System-Text'!
TextEditor subclass: #SmalltalkEditor
	instanceVariableNames: 'lastIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

'From Cuis 5.0 [latest update: #3859] on 5 September 2019 at 12:10:04 pm'!

!TextEditor methodsFor: 'new selection' stamp: 'jmv 9/5/2019 12:06:44'!
messageSendsRanges: aRanges

	selectionStartBlocks := OrderedCollection new.
	selectionStopBlocks := OrderedCollection new.
	
	aRanges do: [ :range | 
		selectionStartBlocks add: (textComposition characterBlockForIndex: range first).
		selectionStopBlocks add: (textComposition characterBlockForIndex: range last + 1) ].
	aRanges notEmpty ifTrue: [
		self selectFrom: aRanges last first to: aRanges last last ]! !


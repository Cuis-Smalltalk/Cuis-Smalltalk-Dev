'From Cuis 5.0 [latest update: #3911] on 10 October 2019 at 9:24:43 am'!

!TextEditor methodsFor: 'new selection' stamp: 'jmv 10/10/2019 09:22:28'!
messageSendsRanges: aRanges
	"aRanges must be notEmpty"
	| lastRange |

	selectionStartBlocks := OrderedCollection new.
	selectionStopBlocks := OrderedCollection new.
	lastRange := nil.

	aRanges do: [ :range |
		selectionStartBlocks add: (textComposition characterBlockForIndex: range first).
		selectionStopBlocks add: (textComposition characterBlockForIndex: range last + 1).
		( lastRange isNil or: [ range first > lastRange first ]) ifTrue: [
			lastRange _ range ]].
	self selectFrom: lastRange first to: lastRange last! !


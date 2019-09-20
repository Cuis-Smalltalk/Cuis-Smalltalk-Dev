'From Cuis 5.0 [latest update: #3878] on 19 September 2019 at 5:33:19 pm'!

!TextEditor methodsFor: 'new selection' stamp: 'jmv 9/19/2019 17:31:32'!
messageSendsRanges: aRanges
	"aRanges must be notEmpty"

	selectionStartBlocks := OrderedCollection new.
	selectionStopBlocks := OrderedCollection new.

	aRanges do: [ :range |
		selectionStartBlocks add: (textComposition characterBlockForIndex: range first).
		selectionStopBlocks add: (textComposition characterBlockForIndex: range last + 1) ].
	self selectFrom: aRanges last first to: aRanges last last! !


!TextModelMorph methodsFor: 'updating' stamp: 'jmv 9/19/2019 17:31:48'!
selectMessage

	| messageSendsRanges |

	messageSendsRanges := model textProvider messageSendsRangesOf: model autoSelectString.
	^ messageSendsRanges notEmpty
		ifTrue: [ self editor messageSendsRanges: messageSendsRanges ]; yourself! !

!TextModelMorph methodsFor: 'updating' stamp: 'jmv 9/19/2019 17:30:20'!
updateAutoSelect 

	TextEditor abandonChangeText.	"no replacement!!"
	self selectMessage
		ifFalse: [ self selectString ].

	self textMorph updateFromTextComposition.
	^self scrollSelectionIntoView! !


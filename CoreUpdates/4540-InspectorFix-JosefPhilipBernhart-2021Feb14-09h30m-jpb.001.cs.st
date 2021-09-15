'From Cuis 5.0 [latest update: #4526] on 14 February 2021 at 10:27:09 am'!

!Inspector methodsFor: 'contents' stamp: 'jpb 2/14/2021 09:59:32'!
acceptedStringOrText
	"We need our cache not to be modified by user changes"
	^(acceptedContentsCache ifNil: '') copy! !

!Inspector methodsFor: 'selecting' stamp: 'jpb 2/14/2021 10:24:54'!
selectionPrintString
	"Returns the current selection as a string"
	^self safelyPrintWith: [
		| selectedValue |
		selectedValue _ self selection.
		(selectedValue is: #String)
			ifTrue: [ selectedValue ]
			ifFalse: [ selectedValue printTextLimitedTo: self printStringLimit]]! !

!methodRemoval: Inspector #stringSelectionIndices stamp: 'jpb 2/14/2021 10:23:57'!
Inspector removeSelector: #stringSelectionIndices!

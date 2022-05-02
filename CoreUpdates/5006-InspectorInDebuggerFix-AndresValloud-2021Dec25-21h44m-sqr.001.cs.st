'From Cuis 5.0 [latest update: #5004] on 25 December 2021 at 9:52:16 pm'!

!Inspector methodsFor: 'accessing' stamp: 'sqr 12/25/2021 21:46:30'!
printedSelectionIndices

	^#(0 2)! !

!Inspector methodsFor: 'testing' stamp: 'sqr 12/25/2021 21:46:14'!
shouldAssumeSelectionIsPrinted

	^self printedSelectionIndices includes: self selectionIndex! !


!ContextVariablesInspector methodsFor: 'accessing' stamp: 'sqr 12/25/2021 21:50:15'!
printedSelectionIndices

	^#(0 3)! !


!Inspector methodsFor: 'selecting' stamp: 'sqr 12/25/2021 21:46:54'!
selectionPrintString
	"Returns the current selection as a string"
	
	^self shouldAssumeSelectionIsPrinted 
		ifTrue: [ self selection ]
		ifFalse: [ self safelyPrintWith: [ self selection printTextLimitedTo: self printStringLimit ] ].! !

!methodRemoval: Inspector #shouldPrintSelectionAsString stamp: 'sqr 12/25/2021 21:47:00'!
Inspector removeSelector: #shouldPrintSelectionAsString!

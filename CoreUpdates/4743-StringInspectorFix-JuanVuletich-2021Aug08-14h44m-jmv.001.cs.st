'From Cuis 5.0 [latest update: #4741] on 8 August 2021 at 2:47:31 pm'!

!Inspector methodsFor: 'selecting' stamp: 'jmv 8/8/2021 14:46:53'!
selectionPrintString
	"Returns the current selection as a string"
	^self safelyPrintWith: [
		self selection printTextLimitedTo: self printStringLimit ].! !


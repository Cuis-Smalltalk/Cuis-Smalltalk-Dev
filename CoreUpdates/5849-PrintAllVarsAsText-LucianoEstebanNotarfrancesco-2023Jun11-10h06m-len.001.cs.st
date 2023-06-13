'From Cuis 6.0 [latest update: #5847] on 11 June 2023 at 10:08:33 am'!

!Object methodsFor: 'printing' stamp: 'len 6/10/2023 16:20:24'!
longPrintText
	"Answer a Text with a description of the receiver."
	
	^ (Text streamContents: [:aStream | self longPrintOn: aStream])
		ifEmpty: [self printText, String newLineString] "Objects without inst vars should return something"! !

!Object methodsFor: 'printing' stamp: 'len 6/10/2023 15:58:12'!
longPrintTextLimitedTo: anInteger
	"Answer a Text with a description of the receiver. limited to anInteger characters."

	^ (Text streamContents: [:aStream | self longPrintOn: aStream limitedTo: anInteger indent: 0])
		ifEmpty: [self printText, String newLineString] "Objects without inst vars should return something"! !


!Object methodsFor: 'printing' stamp: 'len 6/10/2023 16:21:40'!
longPrintOn: aStream limitedTo: sizeLimit indent: indent
	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  Limit is the length limit for each inst var."

	self class allInstVarNames withIndexDo: [ :title :index |
		indent timesRepeat: [aStream tab].
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 nextPutAll: 
			((self instVarAt: index) printTextLimitedTo: (sizeLimit - 3 - title size max: 1));
		 newLine ]! !


!Inspector methodsFor: 'selecting' stamp: 'len 6/10/2023 15:58:23'!
selectionMeta
	"Answer the value of the selected meta field"

	self selectionIndex = 0 ifTrue: [^ ''].
	self selectionIndex = 1 ifTrue: [^ object].
	self selectionIndex = 2 ifTrue: [^ self safelyPrintWith: [object longPrintTextLimitedTo: self printStringLimit]].
	^self selectionMetaUnknown! !


!DebuggerMethodMap methodsFor: 'accessing' stamp: 'len 6/10/2023 15:49:47'!
tempsAndValuesForContext: aContext
	"Return a string of the temporary variabls and their current values"

	^ Text streamContents: [ :strm |
		(self tempNamesForContext: aContext) withIndexDo: [ :title :index |
			strm nextPutAll: title; nextPut: $:; space; tab.
			aContext print: (self namedTempAt: index in: aContext) on: strm.
			strm newLine]].! !


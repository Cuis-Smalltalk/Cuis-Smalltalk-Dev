'From Cuis7.9 [latest update: #8281] on 26 October 2026 at 2:18:47 pm'!

!Object methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 4:51:38 pm (UTC)'!
printForInspectorOn: aStream limitedTo: sizeLimit indent: indent
	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  Limit is the length limit for each inst var."

	self class allInstVarNames withIndexDo: [ :title :index |
		indent timesRepeat: [aStream tab].
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 nextPutAll: 
			((self instVarAt: index) printTextLimitedTo: (sizeLimit - 3 - title size max: 5));
		 newLine ]! !


!UniFileStream methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 4:51:41 pm (UTC)'!
printForInspectorOn: aStream limitedTo: sizeLimit indent: indent

	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."

	aStream newLine! !


!Inspector methodsFor: 'private' stamp: 'jmv 26/Oct/2026, 4:51:50 pm (UTC)'!
longPrintTextOf: anObject limitedTo: anInteger

	^ (Text streamContents: [:aStream | anObject printForInspectorOn: aStream limitedTo: anInteger indent: 0])
		ifEmpty: [anObject printText, String newLineString] "Objects without inst vars should return something"! !


!MethodContext methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 4:51:46 pm (UTC)'!
printDetails: strm
	"Put my class>>selector and instance variables and arguments and temporaries on the stream.  Protect against errors during printing."

	| pe str pos |
	self printOn: strm.
	strm newLine.
	strm tab; nextPutAll: 'Receiver: '.
	pe := '<<error during printing>>'.
	strm nextPutAll: ([receiver shortPrintString] ifError: [:err :rcvr | pe]).

	strm newLine; tab; nextPutAll: 'Arguments and temporary variables: '; newLine.
	str := [(self tempsAndValuesLimitedTo: 160 indent: 2) 
				padded: #right to: 1 with: $x] ifError: [:err :rcvr | pe].
	strm nextPutAll: (str allButLast).

	strm newLine; tab; nextPutAll: 'Receiver''s instance variables: '; newLine.
	pos := strm position.
	[receiver printForInspectorOn: strm limitedTo: 160 indent: 2] ifError: [:err :rcvr | 
				strm nextPutAll: pe].
	pos = strm position ifTrue: ["normal printString for an Array (it has no inst vars)"
		strm nextPutAll: ([receiver shortPrintString] ifError: [:err :rcvr | pe])].
	strm peekLast isLineSeparator ifFalse: [strm newLine].! !


!Inspector methodsFor: 'selecting' stamp: 'jmv 26/Oct/2026, 5:13:23 pm (UTC)'!
selectionMeta
	"Answer the value of the selected meta field"
	self selectionIndex = 0 ifTrue: [^ ''].
	self selectionIndex = 1 ifTrue: [^ object].
	self selectionIndex = 2 ifTrue: [
		^ self safelyPrintWith: [ self longPrintTextOf: object limitedTo: self printStringLimit ]].
	^self selectionMetaUnknown! !


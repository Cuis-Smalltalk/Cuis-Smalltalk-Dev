'From Cuis7.9 [latest update: #8265] on 5 October 2026 at 5:37:03 pm'!

!Object methodsFor: 'printing' stamp: 'FJG 4/Oct/2026, 5:55:01 pm (UTC)'!
longPrintOn: aStream limitedTo: sizeLimit indent: indent
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

!Object methodsFor: 'printing' stamp: 'FJG 4/Oct/2026, 6:48:27 pm (UTC)'!
printAs: streamType limitedTo: limit
	"Answer an instance of streamType whose characters describe the receiver,
	at most limit characters long. If the description is longer, it is cut short
	and ends with '[..]' (the marker counts toward the limit).
	Note that a limit under 4 can still answer the 4 character marker.

	If you want to print without a character limit, use fullPrintString."

	| limitedString |
	limitedString := streamType streamContents: [ :stream | self printOn: stream ] limitedTo: limit + 1.
	limitedString size <= limit ifTrue: [^ limitedString].

	^ (limitedString first: limit - 4), '[..]'! !


!ContextPart methodsFor: 'debugger access' stamp: 'FJG 4/Oct/2026, 6:44:40 pm (UTC)'!
tempsAndValuesLimitedTo: sizeLimit indent: indent
	"Return a string of the temporary variables and their current values"

	| tempNames title |
	^ String streamContents: [ :strm |
		tempNames := self tempNames.
		1 to: self size do: [ :index |
			title := tempNames size = self size ifTrue: [tempNames at: index]  ifFalse: [ 'argOrTemp', index printString ].
			indent timesRepeat: [strm tab].
			strm nextPutAll: title; nextPut: $:; space; tab.
			strm nextPutAll: 
				((self tempAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 5)).
			strm newLine ]].! !


!BreakingMethodWrapper methodsFor: 'printing' stamp: 'FJG 4/Oct/2026, 6:48:27 pm (UTC)'!
printAs: streamType limitedTo: limit
	"Answer an instance of streamType whose characters describe the receiver,
	at most limit characters long. If the description is longer, it is cut short
	and ends with '[..]' (the marker counts toward the limit).
	Note that a limit under 4 can still answer the 4 character marker.

	If you want to print without a character limit, use fullPrintString."

	| limitedString |
	limitedString := streamType streamContents: [ :stream | self printOn: stream ] limitedTo: limit + 1.
	limitedString size <= limit ifTrue: [^ limitedString].

	^ (limitedString first: limit - 4), '[..]'! !


'From Cuis 5.0 [latest update: #5010] on 29 December 2021 at 10:41:01 am'!

!MethodContext methodsFor: 'printing' stamp: 'jmv 12/29/2021 10:40:36'!
printDetails: strm
	"Put my class>>selector and instance variables and arguments and temporaries on the stream.  Protect against errors during printing."

	| pe str pos |
	self printOn: strm.
	strm newLine.
	strm tab; nextPutAll: 'Receiver: '.
	pe _ '<<error during printing>>'.
	strm nextPutAll: ([receiver shortPrintString] ifError: [:err :rcvr | pe]).

	strm newLine; tab; nextPutAll: 'Arguments and temporary variables: '; newLine.
	str _ [(self tempsAndValuesLimitedTo: 160 indent: 2) 
				padded: #right to: 1 with: $x] ifError: [:err :rcvr | pe].
	strm nextPutAll: (str allButLast).

	strm newLine; tab; nextPutAll: 'Receiver''s instance variables: '; newLine.
	pos _ strm position.
	[receiver longPrintOn: strm limitedTo: 160 indent: 2] ifError: [:err :rcvr | 
				strm nextPutAll: pe].
	pos = strm position ifTrue: ["normal printString for an Array (it has no inst vars)"
		strm nextPutAll: ([receiver shortPrintString] ifError: [:err :rcvr | pe])].
	strm peekLast isLineSeparator ifFalse: [strm newLine].! !


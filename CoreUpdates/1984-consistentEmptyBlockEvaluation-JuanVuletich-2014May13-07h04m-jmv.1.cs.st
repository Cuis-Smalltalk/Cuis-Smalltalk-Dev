'From Cuis 4.2 of 25 July 2013 [latest update: #1982] on 13 May 2014 at 7:08:17.862363 am'!

!BlockNode methodsFor: 'initialization' stamp: 'jmv 5/13/2014 07:04'!
arguments: argNodes statements: statementsCollection returns: returnBool from: encoder
	"Compile."
	arguments _ argNodes.
	statements _ statementsCollection size > 0
		ifTrue: [ statementsCollection ]
		ifFalse: [ Array with: NodeNil ].
	optimized _ false.
	returns _ returnBool! !


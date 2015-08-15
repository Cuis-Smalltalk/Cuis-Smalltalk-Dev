'From Cuis 4.2 of 25 July 2013 [latest update: #2455] on 15 August 2015 at 10:22:09.571253 am'!

!Symbol class methodsFor: 'instance creation' stamp: 'jmv 8/15/2015 10:21'!
readFrom: strm
	"
	Symbol readFrom: '#abc' readStream
	"

	strm peek = $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Scanner new scan: strm) advance  "Just do what the code scanner does"! !


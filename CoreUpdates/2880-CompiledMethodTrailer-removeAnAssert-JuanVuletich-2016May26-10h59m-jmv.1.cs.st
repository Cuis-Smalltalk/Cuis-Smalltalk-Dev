'From Cuis 4.2 of 25 July 2013 [latest update: #2797] on 26 May 2016 at 11:00:11.980257 am'!

!CompiledMethodTrailer methodsFor: 'private' stamp: 'jmv 5/26/2016 10:59'!
encodeLengthField: integer

	| bytes value |
"	self assert: (integer > 0)."

	value := integer.
	
	bytes := ByteArray streamContents: [:str |
		[ value > 0 ] whileTrue: [
			str nextPut: (value bitAnd: 255).
			value := value >> 8 ]].

	"no more than 4 bytes for length field"
	self assert: (bytes size <=4).

	^ bytes! !


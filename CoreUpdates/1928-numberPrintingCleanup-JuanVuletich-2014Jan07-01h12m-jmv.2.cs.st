'From Cuis 4.2 of 25 July 2013 [latest update: #1926] on 7 January 2014 at 1:25:56.688236 am'!

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 1/7/2014 01:23'!
storeBitsOn: aStream base: anInteger
	bits do: [ :word | 
		anInteger = 10
			ifTrue: [aStream space]
			ifFalse: [aStream newLineTab: 2].
		anInteger = 2
			ifTrue: [
				"Print binary with radix, but padded, so the bit pattern is easy to see."
				aStream nextPut: $2. 
				aStream nextPut: $r. 
				word printOn: aStream base: 2 length: 32 padded: true ]
			ifFalse: [
				word storeOn: aStream base: anInteger ]]! !


!Integer methodsFor: 'printing' stamp: 'jmv 1/7/2014 01:13'!
hex
	"Print the receiver as hex, prefixed with 16r.  DO NOT CHANGE THIS!!  The Cog VMMaker depends on this.
	 Consider using any of
		printStringBase: 16
		printStringBase: 16 length: 8 padded: true
		storeStringBase: 16
		storeStringBase: 16 length: 11 padded: true"

	^ String streamContents: [ :strm | self storeOn: strm base: 16 ]! !
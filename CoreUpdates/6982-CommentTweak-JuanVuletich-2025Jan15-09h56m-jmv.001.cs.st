'From Cuis7.3 [latest update: #6981] on 15 January 2025 at 9:58:37 am'!

!Integer methodsFor: 'comparing' stamp: 'jmv 1/15/2025 09:56:58'!
hashMultiply
	self subclassResponsibility! !


!Integer methodsFor: 'comparing' stamp: 'jmv 1/15/2025 09:57:42'!
hash
	"Hash is reimplemented because = is implemented.
	Siimply answering self as #hash has bad performance for the following:
	| s |
	s := (1 to: 10000) asSet.
	[s includes: 123456] bench
	"

	"Can only happen in 64 bits images...
	See Float>>#hash, LargePositiveInteger>>#hash and LargeNegativeInteger>>#hash"
	self abs < Float maxExactInteger ifFalse: [
		^ self asFloat hash ].

	"In 32 bit image it will always go this way"
	^self hashMultiply! !


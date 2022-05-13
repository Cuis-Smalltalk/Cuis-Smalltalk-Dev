'From Cuis 6.0 [latest update: #5162] on 13 May 2022 at 8:38:50 pm'!

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/13/2022 20:37:37'!
withBytes: aByteArray
	"Remove a possible and useless BOM (byte order mark)"
	
	| bom bytesToUse |
	bom _ `Character utf8BytesOfUnicodeCodePoint: 16rFEFF`.
	bytesToUse _ (aByteArray beginsWith: bom)
		ifFalse: [aByteArray]
		ifTrue: [aByteArray copyFrom: bom size+1 to: aByteArray size].
	^self new bytes: bytesToUse ! !


'From Cuis7.3 [latest update: #7173] on 19 May 2025 at 10:40:45 am'!

!Bitmap methodsFor: 'accessing' stamp: 'ra 5/19/2025 10:40:21'!
byteAt: byteAddress bigEndian: isBigEndian
	"Extract a byte from a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:.  See Form pixelAt:  7/1/96 tk"
	| lowBits shift |
	lowBits := byteAddress - 1 bitAnd: 3.
	shift := isBigEndian 
		ifTrue: [ (lowBits - 3) * 8 ]
		ifFalse: [ (0 - lowBits) * 8 ].
	^((self at: byteAddress - 1 - lowBits // 4 + 1)
		bitShift: shift)
		bitAnd: 16rFF! !


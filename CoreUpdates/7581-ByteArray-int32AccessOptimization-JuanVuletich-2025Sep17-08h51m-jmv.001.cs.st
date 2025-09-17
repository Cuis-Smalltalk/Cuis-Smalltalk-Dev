'From Cuis7.5 [latest update: #7580] on 17 September 2025 at 8:52:33 am'!

!ByteArray methodsFor: 'accessing - Numeric Types' library: #MinimalKernel01 stamp: 'jmv 9/17/2025 08:51:03'!
int32At: index bigEndian: bigEndian
	"Return a 32-bit integer quantity starting from the given byte index.
	Answer an instance of SmallInteger or LargePositiveInteger as required.
	Avoid allocation of extra LargePositiveIntegers"
	
	| byte result |
	bigEndian ifFalse: [
		(byte := self at: index + 3) <= 16r7F ifTrue: [ "Is the result non-negative?"
			(byte <= 16r3F or: [SmallInteger maxVal >  1073741823]) ifTrue: [
				^(((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index) ].
			^(LargePositiveInteger new: 4)
				replaceFrom: 1
					to: 4
					with: self
					startingAt: index;
				normalize ].
		"Negative"
		(byte >= 16rC0 or: [SmallInteger maxVal >  1073741823]) ifTrue: [
			^-1 - (((((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitXor: 16rFFFFFF) bitShift: 8) + ((self at: index) bitXor: 16rFF)) ].
		(result := LargeNegativeInteger new: 4)
			digitAt: 4 put: ((self at: index + 3) bitXor: 16rFF);
			digitAt: 3 put: ((self at: index + 2) bitXor: 16rFF);
			digitAt: 2 put: ((self at: index + 1) bitXor: 16rFF).
		(byte := ((self at: index) bitXor: 16rFF) + 1) <= 16rFF ifTrue: [
			^result
				digitAt: 1 put: byte;
				normalize ].
		^result
			digitAt: 1 put: 16rFF;
			- 1 "It's tempting to do the subtraction in a loop to avoid the LargeInteger creation, but it's actually slower than this." ].
	(byte := self at: index) <= 16r7F ifTrue: [ "Is the result non-negative?"
		(byte <= 16r3F or: [SmallInteger maxVal >  1073741823]) ifTrue: [
			^(((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 3) ].
		^(LargePositiveInteger new: 4)
			digitAt: 1 put: (self at: index + 3);
			digitAt: 2 put: (self at: index + 2);
			digitAt: 3 put: (self at: index + 1);
			digitAt: 4 put: byte;
			normalize ].
	"Negative"
	(16rC0 <= byte or: [SmallInteger maxVal >  1073741823]) ifTrue: [
		^-1 - (((((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitXor: 16rFFFFFF) bitShift: 8) + ((self at: index + 3) bitXor: 16rFF)) ].
	(result := LargeNegativeInteger new: 4)
		digitAt: 4 put: (byte bitXor: 16rFF);
		digitAt: 3 put: ((self at: index + 1) bitXor: 16rFF);
		digitAt: 2 put: ((self at: index + 2) bitXor: 16rFF).
	(byte := ((self at: index + 3) bitXor: 16rFF) + 1) <= 16rFF ifTrue: [
		^result
			digitAt: 1 put: byte;
			normalize ].
	^result 
		digitAt: 1 put: 16rFF;
		- 1 "It's tempting to do the subtraction in a loop to avoid the LargeInteger creation, but it's actually slower than this."! !


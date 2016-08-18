'From Cuis 4.2 of 25 July 2013 [latest update: #2880] on 29 July 2016 at 3:30:56 pm'!

!LargePositiveInteger methodsFor: 'converting' stamp: 'tfel 2/19/2016 12:43'!
normalize
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen |
	<primitive: 'primNormalizePositive' module:'LargeIntegers'>
	"First establish len = significant length"
	len := oldLen := self digitLength.
	[len = 0 ifTrue: [^0].
	(self digitAt: len) = 0]
		whileTrue: [len := len - 1].

	"Now check if in SmallInteger range.  Fast compute SmallInteger maxVal digitLength"
	sLen := SmallInteger maxVal > 16r7FFFFFFF "we're definitely on 64bit if we are larger than (2 raisedTo: 31) - 1"
				ifTrue: [8]
				ifFalse: [4].
	(len <= sLen
	 and: [(self digitAt: sLen) <= (SmallInteger maxVal digitAt: sLen)])
		ifTrue: ["If so, return its SmallInt value"
				val := 0.
				len to: 1 by: -1 do:
					[:i | val := (val *256) + (self digitAt: i)].
				^ val].

	"Return self, or a shortened copy"
	len < oldLen
		ifTrue: [^ self growto: len]
		ifFalse: [^ self]! !


!LargeNegativeInteger methodsFor: 'converting' stamp: 'tfel 2/19/2016 12:43'!
normalize
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen minVal |
	<primitive: 'primNormalizeNegative' module: 'LargeIntegers'>
	"First establish len = significant length"
	len := oldLen := self digitLength.
	[len = 0 ifTrue: [^0].
	(self digitAt: len) = 0]
		whileTrue: [len := len - 1].

	"Now check if in SmallInteger range.
	 Fast compute SmallInteger minVal digitLength"
	sLen := SmallInteger minVal < -16r80000000 "we're definitely on 64bit if we are smaller than (-2 raisedTo: 31)"
				ifTrue: [8]
				ifFalse: [4].
	len <= sLen ifTrue:
		[minVal := SmallInteger minVal.
		(len < sLen
		 or: [(self digitAt: sLen) < minVal lastDigit])
			ifTrue: ["If high digit less, then can be small"
					val := 0.
					len to: 1 by: -1 do:
						[:i | val := (val *256) - (self digitAt: i)].
					^ val].
		1 to: sLen do:  "If all digits same, then = minVal"
			[:i | (self digitAt: i) = (minVal digitAt: i)
					ifFalse: ["Not so; return self shortened"
							len < oldLen
								ifTrue: [^ self growto: len]
								ifFalse: [^ self]]].
		^ minVal].

	"Return self, or a shortened copy"
	len < oldLen
		ifTrue: [^ self growto: len]
		ifFalse: [^ self]! !


!SmallInteger methodsFor: 'printing' stamp: 'ul 4/2/2016 20:19'!
decimalDigitLength
	"Answer the number of digits printed out in base 10.
	 Note that this only works for positive SmallIntegers up to 63-bits."
	
	self <= 99999999 "8" ifTrue: [
		self <= 9999 "4" ifTrue: [
			self <= 99 "2" ifTrue: [
				self <= 9 "1" ifTrue: [ ^1].
				^2 ].
			self <= 999 "3" ifTrue: [ ^3].
			^4 ].
		self <= 999999 "6" ifTrue: [
			self <= 99999 "5" ifTrue: [ ^5 ].
			^6 ].
		self <= 9999999 "7" ifTrue: [ ^7 ].
		^8 ].
	self <= 1073741823 "10" ifTrue: [ "This is here only to avoid LargeInteger comparisons in 32-bit VMs"
		self <= 999999999 "9" ifTrue: [ ^9 ].
		^10 ].
	self <= 999999999999999 "15" ifTrue: [
		self <= 9999999999999 "13" ifTrue: [
			self <= 99999999999 "11" ifTrue: [
				self <= 9999999999 "10" ifTrue: [ ^10 ].
				^11 ].
			self <= 999999999999 "12" ifTrue: [ ^12 ].
			^13 ].
		self <= 99999999999999 "14" ifTrue: [ ^14 ].
		^15 ].
	self <= 99999999999999999 "17" ifTrue: [
		self <= 9999999999999999 "16" ifTrue: [ ^16 ].
		^17 ].
	self <= 999999999999999999 "18" ifTrue: [ ^18 ].
	^19 "(1 << 60 - 1) asString size"! !


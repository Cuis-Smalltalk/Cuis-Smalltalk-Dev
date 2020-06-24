'From Cuis 5.0 [latest update: #4235] on 24 June 2020 at 3:55:33 pm'!

!LargePositiveInteger methodsFor: 'bit manipulation' stamp: 'jmv 6/24/2020 11:23:30'!
bitReverse: highBit 
	"This implementation is faster than super"
	
	| digitSize reversed adjust |
	highBit < self highBit ifTrue: [ self error: 'Not enough bits.' ].
	digitSize := highBit + 7 // 8.
	reversed := self class new: digitSize.
	1 to: self digitLength do: [:i |
		reversed digitAt: digitSize + 1 - i put: (self digitAt: i) byteReversed].
	adjust _ highBit - (digitSize * 8).
	^adjust = 0
		ifTrue: [reversed normalize]
		ifFalse: [reversed bitShift: adjust]! !


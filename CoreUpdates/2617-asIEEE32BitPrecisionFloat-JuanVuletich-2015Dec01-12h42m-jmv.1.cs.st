'From Cuis 4.2 of 25 July 2013 [latest update: #2595] on 1 December 2015 at 2:48:41.47579 pm'!

!Float64Array methodsFor: 'converting' stamp: 'jmv 11/26/2015 15:06'!
asIEEE32BitPrecisionFloat
	| answer s |
	self class == Float64Array ifFalse: [
		self error: 'please implement' ].
	s _ self size.
	answer _ FloatArray new: s.
	1 to: s do: [ :i | answer at: i put: (self at: i) ].
	^answer! !


!Float methodsFor: 'converting' stamp: 'jmv 12/1/2015 13:02'!
asIEEE32BitPrecisionFloat
	"Answer a 64-bit Float, but using only the precision of a 32-bit Float, as used in FloatArray and 'float' in the C world."
	
	^ Float fromIEEE32Bit: self asIEEE32BitWord! !


'From Cuis 6.0 [latest update: #5761] on 12 April 2023 at 7:24:11 pm'!

!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 4/12/2023 15:02:46'!
bytecodes
	"Answer a ByteArray with my bytecodes."

	| initialPC answer |
	initialPC := self initialPC.
	answer := ByteArray new: self endPC-initialPC+1.
	1 to: answer size do: [ :i |
		answer at: i put: (self at: i-1+initialPC) ].
	^answer! !

!CompiledMethod methodsFor: 'literals' stamp: 'jmv 4/12/2023 14:53:05'!
regularLiterals
	"Answer an Array with the regular literals. I.e. not including the last two, #penultimateLiteral and the #methodClassAssociation"

	| answer |
	answer := Array new: self numLiterals-2.
	1 to: answer size do: [ :i |
		answer at: i put: (self literalAt: i) ].
	^answer! !


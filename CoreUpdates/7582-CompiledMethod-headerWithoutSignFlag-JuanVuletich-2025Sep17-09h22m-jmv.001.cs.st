'From Cuis7.5 [latest update: #7581] on 17 September 2025 at 9:34:25 am'!

!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 9/17/2025 09:24:22'!
headerWithoutSignFlag
	"The sign flag is different in 64 and 32 bit systems.
	Answer the rest of the header, that doesn't depend on that."

	| answer |
	answer := self header.
	answer < 0 ifTrue: [
		answer := answer - SmallInteger minVal ].
	^answer! !


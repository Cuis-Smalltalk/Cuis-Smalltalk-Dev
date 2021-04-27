'From Cuis 5.0 [latest update: #4570] on 27 April 2021 at 3:20:35 pm'!

!Number class methodsFor: 'instance creation' stamp: 'jmv 4/27/2021 15:20:21'!
readFrom: stringOrStream 
	"Answer a number as described on aStream.  The number may
	include a leading radix specification, as in 16rFADE"
	| value base aStream sign |
	aStream _ (stringOrStream isMemberOf: String)
		ifTrue: [ReadStream on: stringOrStream]
		ifFalse: [stringOrStream].
	(aStream nextMatchAll: 'NaN') ifTrue: [^ Float nan].
	sign _ (aStream peekFor: $-) ifTrue: [-1] ifFalse: [aStream peekFor: $+. 1].
	(aStream nextMatchAll: 'Infinity') ifTrue: [^ Float infinity * sign].
	base _ 10.
	aStream peek = $.
		ifTrue: [ value _ 0 ]
		ifFalse: [
			value _ Integer readFrom: aStream base: base.
			(aStream peekFor: $r)
				ifTrue: [
					"<base>r<integer>"
					(base _ value) < 2 ifTrue: [
						base = 1 ifTrue: [ ^Integer readBaseOneFrom: aStream ].
						^self error: 'Invalid radix'].
					(aStream peekFor: $-) ifTrue: [sign _ sign negated].
					value _ Integer readFrom: aStream base: base].
			(aStream peekFor: $x)
				ifTrue: [
					"0x<integer>" "Hexadecimal"
					base _ 16.
					(aStream peekFor: $-) ifTrue: [sign _ sign negated].
					value _ Integer readFrom: aStream base: base]].
	^ self readRemainderOf: value from: aStream base: base withSign: sign.! !


'From Cuis 5.0 of 7 November 2016 [latest update: #3439] on 10 September 2018 at 9:16:29 am'!

!Integer class methodsFor: 'instance creation' stamp: 'GC 9/10/2018 00:18:57'!
readFrom: aStream base: base
	"Answer an instance of one of my concrete subclasses. Initial minus sign
	accepted, and bases > 10 use letters A-Z. Embedded radix specifiers not
	allowed--use Number readFrom: for that. Raises an error if
	there are no digits."

	| digit value neg cc atLeastOneDigitRead |
	
	(aStream atEnd) ifTrue: [ self error: 'At least one digit expected here' ].
	
	neg _ aStream peekFor: $-.
	neg ifFalse: [aStream peekFor: $+].
	value _ 0.
	atLeastOneDigitRead _ false.
	[ aStream atEnd ]
		whileFalse: [
			cc _ aStream next.
			digit _ cc digitValue.
			(digit < 0 or: [digit >= base])
				ifTrue: [
					aStream skip: -1.
					atLeastOneDigitRead ifFalse: [self error: 'At least one digit expected here'].
					^neg
						ifTrue: [value negated]
						ifFalse: [value]].
			value _ value * base + digit.
			atLeastOneDigitRead _ true ].
	neg ifTrue: [^ value negated].
	^ value! !


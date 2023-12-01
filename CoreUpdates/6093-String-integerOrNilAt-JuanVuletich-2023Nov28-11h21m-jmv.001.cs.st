'From Cuis 6.0 [latest update: #6092] on 28 November 2023 at 11:22:30 am'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 11/27/2023 11:22:44'!
integerOrNilAt: firstIndex
	"Answer the integer represented by the leading digits of the receiver,
	or nil if the receiver does not begin with a digit"
	| firstNonDigit |
	(self isEmpty or: [(self at: firstIndex) isDigit not]) ifTrue: [^ nil].
	firstNonDigit := self findFirst: [:m | m isDigit not] startingAt: firstIndex.
	firstNonDigit = 0 ifTrue: [firstNonDigit := self size + 1].
	^ (self copyFrom: 1  to: (firstNonDigit - 1)) asNumber
"
'234Whoopie' initialIntegerOrNil
'wimpy' initialIntegerOrNil
'234' initialIntegerOrNil
'2N' initialIntegerOrNil
'2' initialIntegerOrNil
'  89Ten ' initialIntegerOrNil
'78 92' initialIntegerOrNil
"
! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 11/27/2023 11:24:57'!
initialIntegerOrNil
	"Answer the integer represented by the leading digits of the receiver,
	or nil if the receiver does not begin with a digit"

	^self integerOrNilAt: 1.
"
'234Whoopie' initialIntegerOrNil
'wimpy' initialIntegerOrNil
'234' initialIntegerOrNil
'2N' initialIntegerOrNil
'2' initialIntegerOrNil
'  89Ten ' initialIntegerOrNil
'78 92' initialIntegerOrNil
"
! !


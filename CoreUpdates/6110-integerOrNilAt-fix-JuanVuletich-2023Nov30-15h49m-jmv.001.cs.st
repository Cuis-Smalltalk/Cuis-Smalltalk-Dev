'From Cuis6.0 [latest update: #6108] on 30 November 2023 at 3:50:23 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 11/30/2023 15:33:09'!
integerOrNilAt: firstIndex
	"Answer the integer represented by the leading digits of the receiver,
	or nil if the receiver does not begin with a digit"
	| firstNonDigit |
	(self isEmpty or: [(self at: firstIndex) isDigit not]) ifTrue: [^ nil].
	firstNonDigit := self findFirst: [:m | m isDigit not] startingAt: firstIndex.
	firstNonDigit = 0 ifTrue: [firstNonDigit := self size + 1].
	^ (self copyFrom: firstIndex  to: (firstNonDigit - 1)) asNumber
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


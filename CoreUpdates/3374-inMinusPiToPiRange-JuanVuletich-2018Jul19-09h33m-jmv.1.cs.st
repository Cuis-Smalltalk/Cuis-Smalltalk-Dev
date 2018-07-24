'From Cuis 5.0 of 7 November 2016 [latest update: #3372] on 19 July 2018 at 9:35:19 am'!

!Number methodsFor: 'converting' stamp: 'jmv 7/18/2018 11:03:29'!
inMinusPiToPiRange
	"Avoid conversion to Float if already ok"
	(self > Float pi negated and: [self <= Float pi]) ifTrue: [
		^ self ].
	^ self asFloat inMinusPiToPiRange! !


!Float methodsFor: 'converting' stamp: 'jmv 7/17/2018 15:52:15'!
inMinusPiToPiRange
	"For angles in radians. Add or remove whole turns until we get to the (-Pi .. +Pi] range"
	| answer |
	answer _ self \\ Twopi.
	answer > Pi ifTrue: [
		answer _ answer - Twopi ].
	^ answer! !


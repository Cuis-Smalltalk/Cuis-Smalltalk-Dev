'From Cuis 4.1 of 12 December 2012 [latest update: #1557] on 15 January 2013 at 12:36:03 pm'!

!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 1/15/2013 12:18'!
- operand
	"operand is a DateAndTime or a Duration"

	(operand isKindOf: DateAndTime)
		ifTrue: [
			| lticks rticks |
			lticks _ self asLocal ticks.
			rticks _ operand asLocal ticks.
			^Duration
 				seconds: (SecondsInDay *(lticks first - rticks first)) + 
							(lticks second - rticks second)
 				nanoSeconds: (lticks third - rticks third) ].
	
	^self + operand negated! !

!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 1/15/2013 12:05'!
< comparand
	"comparand conforms to protocol DateAndTime,
	or can be converted into something that conforms."
	| lticks rticks |
	self assert: (comparand isKindOf: DateAndTime).
	offset = comparand offset
		ifTrue: [
			lticks _ self ticks.
			rticks _ comparand ticks]
		ifFalse: [
			lticks _ self asUTC ticks.
			rticks _ comparand asUTC ticks].
	^ lticks first < rticks first
		or: [
			lticks first > rticks first
				ifTrue: [ false ]
				ifFalse: [
					lticks second < rticks second
						or: [
							lticks second > rticks second
								ifTrue: [ false ]
								ifFalse: [ lticks third < rticks third ]]]]! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 1/15/2013 12:19'!
to: anEnd
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd isKindOf: DateAndTime).
	^ Timespan starting: self ending: anEnd! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 1/15/2013 12:19'!
to: anEnd by: aDuration
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd isKindOf: DateAndTime).
	self assert: aDuration class == Duration.
	^ (Schedule starting: self ending: anEnd)
		schedule: (Array with: aDuration);
		yourself.
! !


!Timespan methodsFor: 'squeak protocol' stamp: 'jmv 1/15/2013 12:03'!
start: aDateAndTime
	"Store the start DateAndTime of this timespan"

	self assert: (aDateAndTime isKindOf: DateAndTime).
	start _ aDateAndTime! !

!Timespan methodsFor: 'squeak protocol' stamp: 'jmv 1/15/2013 12:19'!
to: anEnd
	"Answer an Timespan. anEnd must be aDateAndTime"

	self assert: (anEnd isKindOf: DateAndTime).
	^ Timespan starting: self start ending: anEnd! !


!Timespan class methodsFor: 'squeak protocol' stamp: 'jmv 1/15/2013 12:20'!
starting: aDateAndTime duration: aDuration
	self classDefinesDuration ifTrue: [
		self shouldNotImplement ].
	self assert: (aDateAndTime isKindOf: DateAndTime).
	^ self basicNew
 		start: aDateAndTime;
		duration: aDuration;
		yourself! !

!Timespan class methodsFor: 'instance creation' stamp: 'jmv 1/15/2013 12:20'!
starting: startDateAndTime ending: endDateAndTime
	self classDefinesDuration ifTrue: [
		self shouldNotImplement ].
	self assert: (startDateAndTime isKindOf: DateAndTime).
	self assert: (endDateAndTime isKindOf: DateAndTime).
	^ self 
		starting: startDateAndTime 
		duration: endDateAndTime - startDateAndTime! !


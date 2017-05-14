'From Cuis 5.0 of 7 November 2016 [latest update: #3079] on 13 May 2017 at 2:18:26 pm'!

!DateAndTime methodsFor: 'private' stamp: 'jmv 5/13/2017 10:58:22'!
substractDateAndtime: operand
	"operand is a DateAndTime or a Duration"

	| lvalue rvalue |
	offset = operand offset
		ifTrue: [
			lvalue _ self.
			rvalue _ operand ]
		ifFalse: [
			lvalue _ self asUTC.
			rvalue _ operand asUTC ].
	^ Duration
		seconds: (Time secondsInDay *(lvalue julianDayNumber - rvalue julianDayNumber)) + 
					(lvalue secondsSinceMidnight - rvalue secondsSinceMidnight)
		nanoSeconds: lvalue nanoSecond - rvalue nanoSecond! !

!DateAndTime methodsFor: 'private' stamp: 'jmv 5/13/2017 10:58:31'!
substractDuration: operand
	"operand is a DateAndTime or a Duration"

	^self + operand negated! !

!DateAndTime methodsFor: 'double dispatching' stamp: 'jmv 5/13/2017 10:59:45'!
substractFrom: aDateAndTime

	^ aDateAndTime substractDateAndtime: self! !


!Duration methodsFor: 'double dispatching' stamp: 'jmv 5/13/2017 11:03:50'!
substractFrom: aDateAndTimeOrDate

	^aDateAndTimeOrDate substractDuration: self! !


!Timespan methodsFor: 'private' stamp: 'jmv 5/13/2017 11:09:07'!
substractDuration: aDuration

	^self class classDefinesDuration
		ifTrue: [ self class including: start - aDuration ]
		ifFalse: [ self class starting: start - aDuration duration: duration ]! !

!Timespan methodsFor: 'private' stamp: 'jmv 5/13/2017 11:18:25'!
substractTimespan: aTimespan

	aTimespan duration = self duration ifFalse: [
		self error: 'Can not substract Timespans of different duration' ].
	
	^self start substractDateAndtime: aTimespan start! !

!Timespan methodsFor: 'double displatching' stamp: 'jmv 5/13/2017 11:08:17'!
substractFrom: aTimespan

	^ aTimespan substractTimespan: self! !


!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 5/13/2017 11:00:53'!
- operand
	"operand is a DateAndTime or a Duration.
	Double dispatch"

	^ operand substractFrom: self! !


!Timespan methodsFor: 'ansi protocol' stamp: 'jmv 5/13/2017 14:16:21'!
- aDurationOrTimespan

	^ aDurationOrTimespan substractFrom: self! !

!Timespan methodsFor: 'squeak protocol' stamp: 'jmv 5/13/2017 11:13:39'!
includes: operand
	"Operand might be a Timespan or a DateAndtime"

	^ (operand is: #Timespan)
			ifTrue: [ (self includes: operand start)
						and: [ self includes: operand end ] ]
			ifFalse: [ operand between: start and: self end ]! !


!Timespan class methodsFor: 'instance creation' stamp: 'jmv 5/13/2017 11:12:44'!
includingTimespan: aTimespan

	| ending starting |
	starting _ self includingDateAndTime: aTimespan start.
	ending _ self includingDateAndTime: aTimespan end.
	starting = ending ifTrue: [ ^ starting ].
	self error: aTimespan printString, ' can not be included in a ', self name! !


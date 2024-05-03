'From Cuis6.3 [latest update: #6345] on 2 May 2024 at 4:18:37 pm'!

!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 5/2/2024 16:17:47'!
< comparand
	"comparand is a DateAndTime."
	| lvalue rvalue |
	self assert: (comparand is: #dateAndTime).
	offset = comparand offset
		ifTrue: [
			lvalue := self.
			rvalue := comparand ]
		ifFalse: [
			lvalue := self asUTC.
			rvalue := comparand asUTC ].
	^ lvalue julianDayNumber < rvalue julianDayNumber
		or: [
			lvalue julianDayNumber > rvalue julianDayNumber
				ifTrue: [ false ]
				ifFalse: [
					lvalue secondsSinceMidnight < rvalue secondsSinceMidnight
						or: [
							lvalue secondsSinceMidnight > rvalue secondsSinceMidnight
								ifTrue: [ false ]
								ifFalse: [ lvalue nanoSecond < rvalue nanoSecond ]]]]! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 5/2/2024 16:17:54'!
to: anEnd
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd is: #dateAndTime).
	^ Timespan starting: self ending: anEnd! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 5/2/2024 16:17:58'!
to: anEnd by: aDuration
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd is: #dateAndTime).
	self assert: aDuration class == Duration.
	^ (Schedule starting: self ending: anEnd)
		schedule: (Array with: aDuration);
		yourself.
! !


!Timespan methodsFor: 'squeak protocol' stamp: 'jmv 5/2/2024 16:18:03'!
start: aDateAndTime
	"Store the start DateAndTime of this timespan"

	self assert: (aDateAndTime is: #dateAndTime).
	start := aDateAndTime! !

!Timespan methodsFor: 'squeak protocol' stamp: 'jmv 5/2/2024 16:18:18'!
to: anEnd
	"Answer an Timespan. anEnd must be aDateAndTime"

	self assert: (anEnd is: #dateAndTime).
	^ Timespan starting: self start ending: anEnd! !


!Timespan class methodsFor: 'squeak protocol' stamp: 'jmv 5/2/2024 16:18:08'!
starting: aDateAndTime duration: aDuration
	self classDefinesDuration ifTrue: [
		self shouldNotImplement ].
	self assert: (aDateAndTime is: #dateAndTime).
	^ self basicNew
 		start: aDateAndTime;
		duration: aDuration;
		yourself! !

!Timespan class methodsFor: 'instance creation' stamp: 'jmv 5/2/2024 16:18:14'!
starting: startDateAndTime ending: endDateAndTime
	self classDefinesDuration ifTrue: [
		self shouldNotImplement ].
	self assert: (startDateAndTime is: #dateAndTime).
	self assert: (endDateAndTime is: #dateAndTime).
	^ self 
		starting: startDateAndTime 
		duration: endDateAndTime - startDateAndTime! !


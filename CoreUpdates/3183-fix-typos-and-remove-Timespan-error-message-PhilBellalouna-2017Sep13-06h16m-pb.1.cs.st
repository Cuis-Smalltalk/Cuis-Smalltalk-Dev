'From Cuis 5.0 of 7 November 2016 [latest update: #3174] on 13 September 2017 at 6:23:32 am'!

!DateAndTime methodsFor: 'private' stamp: 'pb 9/13/2017 06:17:35'!
subtractDateAndtime: operand
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

!DateAndTime methodsFor: 'private' stamp: 'pb 9/13/2017 06:17:41'!
subtractDuration: operand
	"operand is a DateAndTime or a Duration"

	^self + operand negated! !

!DateAndTime methodsFor: 'double dispatching' stamp: 'pb 9/13/2017 06:20:33'!
subtractFrom: aDateAndTime

	^ aDateAndTime subtractDateAndtime: self! !


!Duration methodsFor: 'double dispatching' stamp: 'pb 9/13/2017 06:21:11'!
subtractFrom: aDateAndTimeOrDate

	^aDateAndTimeOrDate subtractDuration: self! !


!Timespan methodsFor: 'private' stamp: 'pb 9/13/2017 06:18:08'!
subtractDuration: aDuration

	^self class classDefinesDuration
		ifTrue: [ self class including: start - aDuration ]
		ifFalse: [ self class starting: start - aDuration duration: duration ]! !

!Timespan methodsFor: 'private' stamp: 'pb 9/13/2017 06:19:33'!
subtractTimespan: aTimespan
	^self start subtractDateAndtime: aTimespan start! !

!Timespan methodsFor: 'double dispatching' stamp: 'pb 9/13/2017 06:21:26'!
subtractFrom: aTimespan

	^ aTimespan subtractTimespan: self! !


!DateAndTime methodsFor: 'ansi protocol' stamp: 'pb 9/13/2017 06:20:43'!
- operand
	"operand is a DateAndTime or a Duration.
	Double dispatch"

	^ operand subtractFrom: self! !


!Timespan methodsFor: 'ansi protocol' stamp: 'pb 9/13/2017 06:20:49'!
- aDurationOrTimespan

	^ aDurationOrTimespan subtractFrom: self! !


!ScrollBar methodsFor: 'geometry' stamp: 'pb 9/13/2017 06:22:10'!
freeSliderRoom
	"Answer the length or height of the free slider area, i.e. subtract the slider itself.
	If we are really too short of room, lie a little bit. Answering at least 4, even when the
	free space might be actually negative, makes the scrollbar somewhat usable."

	| buttonsRoom |
	buttonsRoom _ Theme current minimalWindows ifTrue: [0] ifFalse: [self buttonExtent * 2].
	^ ((self isHorizontal
		ifTrue: [ extent x - slider morphWidth]
		ifFalse: [ extent y - slider morphHeight])
			- (borderWidth * 2) - buttonsRoom) max: 4! !

!methodRemoval: Timespan #substractDuration:!
Timespan removeSelector: #substractDuration:!
!methodRemoval: Timespan #substractFrom:!
Timespan removeSelector: #substractFrom:!
!methodRemoval: Timespan #substractTimespan:!
Timespan removeSelector: #substractTimespan:!
!methodRemoval: Duration #substractFrom:!
Duration removeSelector: #substractFrom:!
!methodRemoval: DateAndTime #substractDateAndtime:!
DateAndTime removeSelector: #substractDateAndtime:!
!methodRemoval: DateAndTime #substractDuration:!
DateAndTime removeSelector: #substractDuration:!
!methodRemoval: DateAndTime #substractFrom:!
DateAndTime removeSelector: #substractFrom:!

!Timespan reorganize!
('ansi protocol' + - = hash)
('squeak protocol' duration end includes: includesAllOf: includesAnyOf: intersection: printOn: start start: to: union:)
('enumerating' every:do: workDatesDo:)
('private' do:with: do:with:when: duration: subtractDuration: subtractTimespan:)
('smalltalk-80' next previous)
('testing' is:)
('double dispatching' includingTimespanOf: subtractFrom:)
!


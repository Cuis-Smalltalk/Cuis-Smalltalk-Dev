"
I represent a duration starting on a specific DateAndTime.
"
Class {
	#name : 'Timespan',
	#superclass : 'Object',
	#instVars : [
		'start',
		'duration'
	],
	#category : 'Kernel-Chronology',
	#package : 'Kernel',
	#tag : 'Chronology'
}

{ #category : 'inquiries' }
Timespan class >> classDefinesDuration [
	"Answer yes if we must have a fixed duration.
	Examples of this are Date, Month, Week, Year"
	^false
]

{ #category : 'squeak protocol' }
Timespan class >> current [
	"
	Date current
	Week current
	Month current
	"

	^ self including: DateAndTime now
]

{ #category : 'squeak protocol' }
Timespan class >> fromString: aString [
	"Please call with specific subclass."

	^ self readFrom: aString readStream
]

{ #category : 'instance creation' }
Timespan class >> including: aDateAndTime [

	^ aDateAndTime includingTimespanOf: self
]

{ #category : 'instance creation' }
Timespan class >> includingDateAndTime: aDateAndTime [

	^ self starting: aDateAndTime duration: `Duration zero`.
]

{ #category : 'instance creation' }
Timespan class >> includingTimespan: aTimespan [

	| ending starting |
	starting := self includingDateAndTime: aTimespan start.
	ending := self includingDateAndTime: aTimespan end.
	starting = ending ifTrue: [ ^ starting ].
	self error: aTimespan printString, ' can not be included in a ', self name
]

{ #category : 'inquiries' }
Timespan class >> mustEncompassWholeDates [
	"Answer yes if we must start and end at midnight.
	Examples of this are Date, Month, Week, Year"
	^false
]

{ #category : 'squeak protocol' }
Timespan class >> new [
	"Answer a Timespan starting on the Squeak epoch: 1 January 1901"
	"Perhaps you need
		Timespan current
	"
	^ self shouldNotImplement
]

{ #category : 'squeak protocol' }
Timespan class >> starting: aDateAndTime duration: aDuration [
	self classDefinesDuration ifTrue: [
		self shouldNotImplement ].
	self assert: (aDateAndTime is: #dateAndTime).
	^ self basicNew
 		start: aDateAndTime;
		duration: aDuration;
		yourself
]

{ #category : 'instance creation' }
Timespan class >> starting: startDateAndTime ending: endDateAndTime [
	self classDefinesDuration ifTrue: [
		self shouldNotImplement ].
	self assert: (startDateAndTime is: #dateAndTime).
	self assert: (endDateAndTime is: #dateAndTime).
	^ self 
		starting: startDateAndTime 
		duration: endDateAndTime - startDateAndTime
]

{ #category : 'ansi protocol' }
Timespan >> + aDuration [

	^self class classDefinesDuration
		ifTrue: [ self class including: start + aDuration ]
		ifFalse: [ self class starting: start + aDuration duration: duration ]
]

{ #category : 'ansi protocol' }
Timespan >> - aDurationOrTimespan [

	^ aDurationOrTimespan subtractFrom: self
]

{ #category : 'ansi protocol' }
Timespan >> = comparand [
 	self == comparand ifTrue: [ ^ true ].
	^ self class == comparand class 
		and: [ self start = comparand start ]
		and: [ self duration = comparand duration ]
.
]

{ #category : 'private' }
Timespan >> do: aBlock with: aFirstElement [

	self do: aBlock with: aFirstElement when: [ :t | true ].

]

{ #category : 'private' }
Timespan >> do: aBlock with: aFirstElement when: aConditionBlock [

	| element end |
	element := aFirstElement.
	end := self end.
	[ element start <= end ] whileTrue:
	
	[(aConditionBlock value: element)
			ifTrue: [ aBlock value: element ].
		element := element next. ]
]

{ #category : 'squeak protocol' }
Timespan >> duration [
	"Answer the Duration of this timespan"

	^ duration

]

{ #category : 'private' }
Timespan >> duration: aDuration [
	"Set the Duration of this timespan"

	duration := aDuration

]

{ #category : 'squeak protocol' }
Timespan >> end [

	^ self duration totalNanoSeconds = 0
		ifTrue: [ self start ]
		ifFalse: [ self next start - DateAndTime clockPrecision ]
]

{ #category : 'enumerating' }
Timespan >> every: aDuration do: aBlock [

	| element end |
	element := self start.
	end := self end.
	[ element <= end ] whileTrue:
	
	[ aBlock value: element.
		element := element + aDuration. ]

]

{ #category : 'ansi protocol' }
Timespan >> hash [

	^ start hash bitXor: duration hash

]

{ #category : 'squeak protocol' }
Timespan >> includes: operand [
	"Operand might be a Timespan or a DateAndtime"

	^ (operand is: #Timespan)
			ifTrue: [ (self includes: operand start)
						and: [ self includes: operand end ] ]
			ifFalse: [ operand between: start and: self end ]
]

{ #category : 'squeak protocol' }
Timespan >> includesAllOf: aCollection  [
	"Answer whether all the elements of aCollection are in the receiver."

	aCollection do: [:elem | (self includes: elem) ifFalse: [^ false]].
	^ true

]

{ #category : 'squeak protocol' }
Timespan >> includesAnyOf: aCollection  [
	"Answer whether any element of aCollection is included in the receiver"

	aCollection do: [ :elem | (self includes: elem) ifTrue: [^ true]].
	^false

]

{ #category : 'double dispatching' }
Timespan >> includingTimespanOf: aTimespanClass [

	^ aTimespanClass includingTimespan: self
]

{ #category : 'squeak protocol' }
Timespan >> intersection: aTimespan [
	 "Return the Timespan both have in common, or nil"
	"Warning: There's something possibly wrong here. See 
	http://lists.gforge.inria.fr/pipermail/pharo-project/2011-May/048253.html
	
	It seems reasonable to expect this to answer true. However it answers false:
	|start end span|
	start :=DateAndTime now.
	end := start + 1 hour.
	span := Timespan starting: start ending: end.
	(span intersection: span) = span
	"

	 | aBegin anEnd |
	 aBegin := self start max: aTimespan start.
	 anEnd := self end min: aTimespan end.
	 anEnd < aBegin ifTrue: [^nil].

	 ^ Timespan starting: aBegin ending: anEnd
]

{ #category : 'testing' }
Timespan >> is: aSymbol [
	^aSymbol == #Timespan or: [ super is: aSymbol ]
]

{ #category : 'smalltalk-80' }
Timespan >> next [

	^self class classDefinesDuration
		ifTrue: [ self class including: start + duration ]
		ifFalse: [ self class starting: start + duration duration: duration ]
]

{ #category : 'smalltalk-80' }
Timespan >> previous [
	"
	(Month month: 10 year: 2017) previous
	(Year yearNumber: 2016) previous
	"
	^self class classDefinesDuration
		ifTrue: [ self class including: self end - duration ]
		ifFalse: [ self class starting: start - duration duration: duration ]
]

{ #category : 'squeak protocol' }
Timespan >> printOn: aStream [


	super printOn: aStream.
	aStream 
		nextPut: $(;
		print: start;
		nextPut: $D;
		print: duration;
		nextPut: $).

]

{ #category : 'squeak protocol' }
Timespan >> start [
	"Answer the start DateAndTime of this timespan"

	^ start

]

{ #category : 'squeak protocol' }
Timespan >> start: aDateAndTime [
	"Store the start DateAndTime of this timespan"

	self assert: (aDateAndTime is: #dateAndTime).
	start := aDateAndTime
]

{ #category : 'private' }
Timespan >> subtractDuration: aDuration [

	^self class classDefinesDuration
		ifTrue: [ self class including: start - aDuration ]
		ifFalse: [ self class starting: start - aDuration duration: duration ]
]

{ #category : 'double dispatching' }
Timespan >> subtractFrom: aTimespan [

	^ aTimespan subtractTimespan: self
]

{ #category : 'private' }
Timespan >> subtractMonth: aYear [

	^ self subtractTimespan: aYear
]

{ #category : 'private' }
Timespan >> subtractTimespan: aTimespan [
	"
	(Month month: 'March' year: 2017) - (Month month: 'January' year: 2017)
	(Month month: 'February' year: 2017) - (Month month: 'January' year: 2017)

	(Year yearNumber: 2016) - (Year yearNumber: 2015).
	(Year yearNumber: 2017) - (Year yearNumber: 2016).
	(Year yearNumber: 2017) - (Year yearNumber: 2015).

	(Year yearNumber: 2018) - (Date today). 					'Error'.
	"
	aTimespan duration = self duration ifFalse: [
		self error: 'Can not subtract Timespans of different duration' ].
	
	^self start subtractDateAndtime: aTimespan start
]

{ #category : 'private' }
Timespan >> subtractYear: aYear [

	^ self subtractTimespan: aYear
]

{ #category : 'squeak protocol' }
Timespan >> to: anEnd [
	"Answer an Timespan. anEnd must be aDateAndTime"

	self assert: (anEnd is: #dateAndTime).
	^ Timespan starting: self start ending: anEnd
]

{ #category : 'squeak protocol' }
Timespan >> union: aTimespan [
	 "Return the Timespan spanned by both"
	"
	| union timespan |
	timespan := Timespan starting:
					(DateAndTime year: 2003 month: 03 day: 22 hour: 12 minute: 0 second: 0)
						duration: (Duration hours: 100).
	union := timespan union: timespan.
	
	self 
		assert: (union start = timespan start);
		assert: (union duration = timespan duration)
	"

	| aBegin anEnd |

	aBegin := self start min: aTimespan start.
	anEnd := self end max: aTimespan end.
	^ Timespan starting: aBegin ending: anEnd + DateAndTime clockPrecision
]

{ #category : 'enumerating' }
Timespan >> workDatesDo: aBlock [
	"Exclude Saturday and Sunday"

	self do: aBlock with: start date when: [ :d | d dayOfWeek < 6 ].

]

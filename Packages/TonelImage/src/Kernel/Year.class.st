"
I represent a year.
"
Class {
	#name : 'Year',
	#superclass : 'Timespan',
	#category : 'Kernel-Chronology',
	#package : 'Kernel',
	#tag : 'Chronology'
}

{ #category : 'inquiries' }
Year class >> classDefinesDuration [

	^true
]

{ #category : 'squeak protocol' }
Year class >> current [
 
	^ self yearNumber: (DateAndTime now yearNumber)

]

{ #category : 'smalltalk-80' }
Year class >> daysInYear: yearInteger [

	^ 365 + ((self isLeapYear: yearInteger) ifTrue: [1] ifFalse: [0]).

]

{ #category : 'squeak protocol' }
Year class >> includingDateAndTime: aDateAndTime [
	"Answer a calendar year"

	^ self yearNumber: aDateAndTime yearNumber
]

{ #category : 'squeak protocol' }
Year class >> isLeapYear: aYearInteger [


	| adjustedYear |
	adjustedYear := aYearInteger > 0
		ifTrue: [aYearInteger]
		ifFalse: [(aYearInteger + 1) negated].

	"There was no year 0"
	^ ((adjustedYear \\ 4 ~= 0) or: [(adjustedYear \\ 100 = 0) and: [adjustedYear \\ 400 ~= 0]]) not.
]

{ #category : 'smalltalk-80' }
Year class >> leapYear: yearInteger  [

	^ (self isLeapYear: yearInteger)
		ifTrue: [1]
		ifFalse: [0]
]

{ #category : 'inquiries' }
Year class >> mustEncompassWholeDates [

	^true
]

{ #category : 'squeak protocol' }
Year class >> readFrom: aStream  [

	| year sign |
	sign := aStream peek == $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isDigit] whileFalse: [aStream skip].
	year := (Integer readFrom: aStream) * sign.
	^ self yearNumber: year
]

{ #category : 'squeak protocol' }
Year class >> yearNumber: aYear [

	| yearStart |
	yearStart := DateAndTime year: aYear month: 1 day: 1.
	^ self basicNew
 		start: yearStart;
		duration: (Duration days: (self daysInYear: yearStart yearNumber));
		yourself
]

{ #category : 'squeak protocol' }
Year >> daysInMonth [


	self shouldNotImplement 

]

{ #category : 'squeak protocol' }
Year >> daysInYear [

	^ self duration days
]

{ #category : 'squeak protocol' }
Year >> printOn: aStream [

	aStream nextPutAll: 'a Year ('.
	self start yearNumber printOn: aStream.

	aStream nextPutAll: ')'.

]

{ #category : 'double displatching' }
Year >> subtractFrom: aTimespan [

	^ aTimespan subtractYear: self
]

{ #category : 'double displatching' }
Year >> subtractYear: aYear [
	"Years can be subtracted even if one of them is leap and the other isn't."

	^self start subtractDateAndtime: aYear start
]

{ #category : 'public protocol' }
Year >> yearNumber [

	^ start yearNumber
]

"
I represent a week.
"
Class {
	#name : 'Week',
	#superclass : 'Timespan',
	#category : 'Kernel-Chronology',
	#package : 'Kernel',
	#tag : 'Chronology'
}

{ #category : 'inquiries' }
Week class >> classDefinesDuration [

	^true
]

{ #category : 'inquiries' }
Week class >> dayNames [

	^ #(Monday Tuesday Wednesday Thursday Friday Saturday Sunday)
]

{ #category : 'squeak protocol' }
Week class >> includingDateAndTime: aDateAndTime [
	"
	Week including: '12 May 2017 ' asDate start
	(Week including: '12 May 2017 ' asDate start) start dayOfWeekName = #Monday 
	"

	| midnight weekStart |
	midnight := aDateAndTime midnight.
	weekStart := midnight - (midnight dayOfWeek - 1) days.

	^ self basicNew
 		start: weekStart;
		duration: (Duration weeks: 1);
		yourself
]

{ #category : 'squeak protocol' }
Week class >> indexOfDay: aSymbol [
	"
	(Week indexOfDay: #Sunday) = 7
	(Week nameOfDay: 7) = #Sunday 
	"

	^ self dayNames indexOf: aSymbol
]

{ #category : 'inquiries' }
Week class >> mustEncompassWholeDates [

	^true
]

{ #category : 'smalltalk-80' }
Week class >> nameOfDay: anIndex [
	"
	Week indexOfDay: #Sunday = 7
	Week nameOfDay: 7 = #Sunday 
	"

	^ self dayNames at: anIndex
]

{ #category : 'squeak protocol' }
Week class >> readFrom: aStream  [
	"Read a Week from the stream in any of the forms:  
		<year>-W<week>			(2009-W01) (ISO8601)"
	| weekNumber yearNumber firstAsNumber firstAsNumberSign |
	firstAsNumberSign := aStream peek == $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 12])
			ifTrue: [yearNumber := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream next == $W ifFalse: [
		self error: 'Invalid Format' ].

	weekNumber := Integer readFrom: aStream.
	weekNumber < 1 ifTrue: [ self error: 'Invalid Format' ].
	(yearNumber < 100 and: [yearNumber >= 0])  ifTrue: [
		yearNumber := yearNumber < 69 
			ifTrue: [2000 + yearNumber]
			ifFalse: [1900 + yearNumber]].

	^ self yearNumber: yearNumber weekNumber: weekNumber
]

{ #category : 'squeak protocol' }
Week class >> yearNumber: yearNumber weekNumber: weekNumber [

	| firstOfJanuary firstThursday thisThursday |
	firstOfJanuary := DateAndTime year: yearNumber month: 1 day: 1.
	firstThursday := firstOfJanuary + (4 - firstOfJanuary dayOfWeek \\ 7) days.
	thisThursday := firstThursday + ((weekNumber-1) * 7) days.

	thisThursday yearNumber = yearNumber
		ifFalse: [ self error: 'Week does not exist' ].

	^ self including: thisThursday
]

{ #category : 'squeak protocol' }
Week >> printOn: aStream [
	"
		'2008-W52' asWeek.
		'2009-W01' asWeek
		'2009-W02' asWeek
		'2009-W53' asWeek
		'2010-W01' asWeek
		'2010-W02' asWeek
	"
	self yearNumber printOn: aStream.
	aStream nextPutAll: '-W'.
	self weekNumber printOn: aStream length: 2 zeroPadded: true
]

{ #category : 'public protocol' }
Week >> weekNumber [

	| thursday |
	thursday := self start + 3 days.
	^thursday dayOfYear-1 // 7 + 1
]

{ #category : 'public protocol' }
Week >> yearNumber [

	| thursday |
	thursday := self start + 3 days.
	^thursday yearNumber
]

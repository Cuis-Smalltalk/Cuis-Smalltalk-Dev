Class {
	#name : 'DateTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Chronology',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Chronology'
}

{ #category : 'tests' }
DateTest >> testCreation [

	| date |
	date := '2014/6/30' asDate.
	self assert: date yearNumber = 2014.
	self assert: date monthIndex = 6.
	self assert: date dayOfMonth = 30.

	[ date := '70/12/30' asDate ] on: BackCompatibilityAlert do: [ :ex | ex resume ].
	self assert: date yearNumber = 70.
	self assert: date monthIndex = 12.
	self assert: date dayOfMonth = 30.

	date := '1970/12/30' asDate.
	self assert: date yearNumber = 1970.
	self assert: date monthIndex = 12.
	self assert: date dayOfMonth = 30.

	date := '12/30/1970' asDate.
	self assert: date yearNumber = 1970.
	self assert: date monthIndex = 12.
	self assert: date dayOfMonth = 30.

	self should: ['30/12/1970' asDate] raise: Error.

	[ date := '4/5/6' asDate ] on: BackCompatibilityAlert do: [ :ex | ex resume ].
	self assert: date yearNumber = 4.
	self assert: date monthIndex = 5.
	self assert: date dayOfMonth = 6.

	date := '4/5/2006' asDate.
	self assert: date yearNumber = 2006.
	self assert: date monthIndex = 4.
	self assert: date dayOfMonth = 5.

	date := '15 April 1982' asDate.
	self assert: date yearNumber = 1982.
	self assert: date monthIndex = 4.
	self assert: date dayOfMonth = 15.
]

{ #category : 'test operations' }
DateTest >> testInvalidOperations [

	self should: [ Date today - DateAndTime tomorrow ] raise: Error.
	self should: [ Date today - Week current ] raise: Error.
]

{ #category : 'test operations' }
DateTest >> testLessDate [

	self assert: Date today - Date yesterday = 1 days.
	self assert: Date today - Date tomorrow = -1 days.
	self assert: Date tomorrow start - Date yesterday start = 2 days.
]

{ #category : 'test operations' }
DateTest >> testLessDuration [

	self assert: '13 May 2017' asDate - 3 days = '10 May 2017' asDate
]

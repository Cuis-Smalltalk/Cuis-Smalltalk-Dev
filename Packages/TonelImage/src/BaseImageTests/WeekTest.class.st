Class {
	#name : 'WeekTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Chronology',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Chronology'
}

{ #category : 'tests' }
WeekTest >> testCreation [

	| week |
	week _ '2008-W52' asWeek.
	self assert: week yearNumber = 2008.
	self assert: week weekNumber = 52.
	self assert: week start = '2008-12-22' asDate start.

	self should: [ '2008-W53' asWeek ] raise: Error.
	self should: [ '2009-W54' asWeek ] raise: Error.

	week _ '2009-W01' asWeek.
	self assert: week yearNumber = 2009.
	self assert: week weekNumber = 1.
	self assert: week start = '2008-12-29' asDate start.

	week _ '2009-W02' asWeek.
	self assert: week yearNumber = 2009.
	self assert: week weekNumber = 2.
	self assert: week start = '2009-1-5' asDate start.

	week _ '2009-W52' asWeek.
	self assert: week yearNumber = 2009.
	self assert: week weekNumber = 52.
	self assert: week start = '2009-12-21' asDate start.

	week _ '2009-W53' asWeek.
	self assert: week yearNumber = 2009.
	self assert: week weekNumber = 53.
	self assert: week start = '2009-12-28' asDate start.

	week _ '2010-W01' asWeek.
	self assert: week yearNumber = 2010.
	self assert: week weekNumber = 1.
	self assert: week start = '2010-1-4' asDate start.

	week _ '2010-W02' asWeek.
	self assert: week yearNumber = 2010.
	self assert: week weekNumber = 2.
	self assert: week start = '2010-1-11' asDate start.
]

{ #category : 'tests' }
WeekTest >> testIncluding [

	| week |
	week _ Week including: '12 May 2017 ' asDate start.
	self assert: week = '2017-W19' asWeek.
	self assert: week start dayOfWeekName = #Monday.

	week _ Week including: '12 May 2017 ' asDate.
	self assert: week = '2017-W19' asWeek.
	self assert: week start dayOfWeekName = #Monday.

	week _ Week including: '2010/1/3' asDate start.
	self assert: week = '2009W53' asWeek.
	self assert: week start dayOfWeekName = #Monday.

	week _ Week including: '2010/1/3' asDate.
	self assert: week = '2009W53' asWeek.
	self assert: week start dayOfWeekName = #Monday.

	self should: [ Week including: (Year including: Date today) ] raise: Error
]

{ #category : 'tests' }
WeekTest >> testInvalidOperations [

	self should: [ Week current - Date tomorrow ] raise: Error.
	self should: [ Week current - DateAndTime tomorrow ] raise: Error.
]

{ #category : 'tests' }
WeekTest >> testPrintString [

	{
		'2008-W52'.
		'2009-W01'.
		'2009-W02'.
		'2009-W52'.
		'2009-W53'.
		'2010-W01'.
		'2010-W02'.
	} do: [ :string |
		self assert: string asWeek printString = string ]
]

{ #category : 'test class methods' }
WeekTest >> testWeekDayNames [

	self assert: (Week indexOfDay: #Sunday) = 7.
	self assert: (Week nameOfDay: 7) = #Sunday
]

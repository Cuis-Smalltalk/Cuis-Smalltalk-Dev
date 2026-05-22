Class {
	#name : 'DateAndTimeTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Chronology',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Chronology'
}

{ #category : 'tests' }
DateAndTimeTest >> testCreation [
	"
	self new testCreation
	"
	| d |
	d _ (DateAndTime julianDayNumber: 2456814) date.
	self assert: d yearNumber = 2014.
	self assert: d monthIndex = 6.
	self assert: d dayOfMonth = 5.
	
	d _ (DateAndTime fromSeconds: 3579379200) date.
	self assert: d yearNumber = 2014.
	self assert: d monthIndex = 6.
	self assert: d dayOfMonth = 5.
	
	d _ DateAndTime fromSeconds: 3579431284.
	self assert: d yearNumber = 2014.
	self assert: d monthIndex = 6.
	self assert: d dayOfMonth = 5.
	self assert: d hour = 14.
	self assert: d minute = 28.
	self assert: d second = 4.
	self assert: d nanoSecond = 0
]

{ #category : 'tests' }
DateAndTimeTest >> testDayOfWeek [

	self assert: '13 May 2017 ' asDate dayOfWeek = 6.
	self assert: '13 May 2017 ' asDate dayOfWeekName = #Saturday
]

{ #category : 'tests' }
DateAndTimeTest >> testInvalidOperations [

	self should: [ DateAndTime tomorrow - Week current ] raise: Error.
	self should: [ DateAndTime tomorrow - Date today ] raise: Error
]

{ #category : 'tests' }
DateAndTimeTest >> testIsDateAndTime [
	self assert: (DateAndTime now is: #DateAndTime).
]

{ #category : 'tests' }
DateAndTimeTest >> testLessDateAndTime [

	self assert: DateAndTime today - DateAndTime yesterday = 1 days.
	self assert: DateAndTime today - DateAndTime tomorrow = -1 days.
	self assert: DateAndTime tomorrow - DateAndTime yesterday = 2 days.
]

{ #category : 'tests' }
DateAndTimeTest >> testOperations [
	"
	self new testOperations
	"
	| d dt |
	dt _ DateAndTime now.

	d _ 1 hours.
	self assert: dt < (dt + d).
	self assert: dt - d < dt.
	self assert: dt + d - d = dt.

	d _ 1 minutes.
	self assert: dt < (dt + d).
	self assert: dt - d < dt.
	self assert: dt + d - d = dt.

	d _ 1 seconds.
	self assert: dt < (dt + d).
	self assert: dt - d < dt.
	self assert: dt + d - d = dt.

	d _ -1 hours.
	self assert: dt < (dt - d).
	self assert: dt + d < dt.
	self assert: dt + d - d = dt.

	d _ -1 minutes.
	self assert: dt < (dt - d).
	self assert: dt + d < dt.
	self assert: dt + d - d = dt.

	d _ -1 seconds.
	self assert: dt < (dt - d).
	self assert: dt + d < dt.
	self assert: dt + d - d = dt.
]

{ #category : 'tests' }
DateAndTimeTest >> testToByDo [
	"
	DateAndTimeTest new testToByDo
	"
	| end last start step |
	start _ DateAndTime midnight.
	end _ DateAndTime midnight + 2 hours.
	step _ 15 minutes.
	start to: end by: step do: [ :time | last _ time ].
	self assert: last = end
]

Class {
	#name : 'TimespanTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Chronology',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Chronology'
}

{ #category : 'testing' }
TimespanTest >> testIsTimespan [
	self assert: (Timespan current is: #Timespan).
]

Class {
	#name : 'DurationTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Chronology',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Chronology'
}

{ #category : 'tests' }
DurationTest >> testPrintString [

	self assert: (Duration days: 2 hours: 3 minutes: 16 seconds: 43) printString =  '2:03:16:43' 
]

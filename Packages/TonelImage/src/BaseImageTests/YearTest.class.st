Class {
	#name : 'YearTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Chronology',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Chronology'
}

{ #category : 'tests' }
YearTest >> testCreation [

	self assert: (Year yearNumber: 1998) yearNumber = 1998.
	self assert: '2008' asYear yearNumber = 2008.
	self assert: '2008' asYear start = '2008/01/01' asDate start
]

{ #category : 'tests' }
YearTest >> testIncluding [

	self assert: (Year including: '12 May 2017 ' asDate start) yearNumber = 2017.
	self assert: (Year including: (Week including: '12 May 2017 ' asDate start)) yearNumber = 2017
]

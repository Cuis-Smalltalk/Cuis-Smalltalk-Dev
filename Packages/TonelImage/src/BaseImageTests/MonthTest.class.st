Class {
	#name : 'MonthTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Chronology',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Chronology'
}

{ #category : 'tests' }
MonthTest >> testCreation [

	self assert: 'July 1998' asMonth yearNumber = 1998.
	self assert: 'July 1998' asMonth monthIndex = 7.
	self assert: 'July 1998' asMonth monthName = #July.

	self assert: '1998/7' asMonth yearNumber = 1998.
	self assert: '1998/7' asMonth monthIndex = 7.
	self assert: '1998/7' asMonth monthName = #July.
]

{ #category : 'tests' }
MonthTest >> testPrintString [

	self assert: 'July 1998' asMonth printString = 'July 1998'.
	self assert: '1998/7' asMonth printString = 'July 1998'.

]

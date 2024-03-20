'From Cuis6.3 [latest update: #6282] on 20 March 2024 at 12:27:30 pm'!
!classDefinition: #TestCase category: #'Tools-Testing'!
Object subclass: #TestCase
	instanceVariableNames: 'testSelector failureString raisedError assertCount '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!

!TestCase methodsFor: 'running - private' stamp: 'mir 3/20/2024 15:53:46'!
incrementAssertionCount
	assertCount := assertCount + 1! !


!TestResult methodsFor: 'printing' stamp: 'mir 3/20/2024 17:11:11'!
description
	" Return the description of the receiver. "

	| stream |
	stream := '' writeStream.
	"stream lf; nextPutAll: 'passed:'; lf.
	self passed do: [:stage|
		stream lf;nextPutAll: '	',stage
	]."
	stream lf; nextPutAll: 'failures:';lf.
	self failures do: [:testCase |
		stream lf;nextPutAll: '	',testCase selector ,'	' ,testCase failureString
	].
	stream lf; nextPutAll: 'errors:';lf.
	self errors do: [:testCase|
		stream lf;
			nextPutAll: '	',testCase selector ,'	' ,(testCase failureString ifNil: ['']); lf;
			"nextPutAll: tuple first stack;"
			yourself
	].
	stream lf; nextPutAll: 'passed: '; nextPutAll: self correctCount asString; lf.
	stream nextPutAll: 'failures: '; nextPutAll: self failureCount asString; lf.
	stream nextPutAll: 'errors: '; nextPutAll: self errorCount asString; lf.
	^stream contents! !


!TestCase methodsFor: 'assertions' stamp: 'mir 3/20/2024 15:48:23'!
assert: aBooleanOrBlock

	self assert: aBooleanOrBlock description: nil
			! !

!TestCase methodsFor: 'assertions' stamp: 'mir 3/20/2024 16:08:50'!
assert: aBooleanOrBlock description: aStringOrBlock
	self incrementAssertionCount.
	aBooleanOrBlock value ifFalse: [
		failureString := String streamContents: [:stream |
			stream 	nextPutAll: 'Assertion ('.
			assertCount printOn: stream.
			stream nextPutAll: ') failed. '.
			aStringOrBlock value ifNotNil: [:description | stream nextPutAll: description]].
		self logFailure: failureString.
		TestResult failure sunitSignalWith: failureString]! !

!TestCase methodsFor: 'assertions' stamp: 'mir 3/20/2024 16:10:36'!
deny: aBooleanOrBlock

	self deny: aBooleanOrBlock description: nil! !

!TestCase methodsFor: 'assertions' stamp: 'mir 3/20/2024 16:11:03'!
deny: aBooleanOrBlock description: aStringOrBlock

	self assert: aBooleanOrBlock value not description: aStringOrBlock
			! !

!TestCase methodsFor: 'setUp/tearDown' stamp: 'mir 3/20/2024 15:54:16'!
setUp
	assertCount := 0! !

!classDefinition: #TestCase category: #'Tools-Testing'!
Object subclass: #TestCase
	instanceVariableNames: 'testSelector failureString raisedError assertCount'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!

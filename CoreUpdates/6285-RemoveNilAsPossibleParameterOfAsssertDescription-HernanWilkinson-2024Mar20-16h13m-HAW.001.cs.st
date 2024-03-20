'From Cuis6.3 [latest update: #6284] on 20 March 2024 at 4:15:59 pm'!

!TestCase methodsFor: 'assertions' stamp: 'HAW 3/20/2024 16:13:51'!
assert: aBooleanOrBlock

	self assert: aBooleanOrBlock description: ''
			! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 3/20/2024 16:14:30'!
assert: aBooleanOrBlock description: aStringOrBlock
	self incrementAssertionCount.
	aBooleanOrBlock value ifFalse: [
		failureString := String streamContents: [:stream |
			stream 	nextPutAll: 'Assertion ('.
			assertCount printOn: stream.
			stream nextPutAll: ') failed. '.
			stream nextPutAll: aStringOrBlock value].
		self logFailure: failureString.
		TestResult failure sunitSignalWith: failureString]! !

!TestCase methodsFor: 'assertions' stamp: 'HAW 3/20/2024 16:14:39'!
deny: aBooleanOrBlock

	self deny: aBooleanOrBlock description: ''! !


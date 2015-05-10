'From Cuis 4.2 of 25 July 2013 [latest update: #2315] on 10 May 2015 at 3:29:27.22328 pm'!

!Object methodsFor: 'error handling' stamp: 'jmv 5/10/2015 15:28'!
assert: aBlock description: aStringOrBlock
    "Throw an assertion error if aBlock does not evaluates to true."

    aBlock value 
        ifFalse: [ AssertionFailure signal: aStringOrBlock value ]! !


!TestCase methodsFor: 'Accessing' stamp: 'jmv 5/10/2015 15:29'!
assert: aBoolean description: aStringOrBlock
	aBoolean ifFalse: [
		failureString _ aStringOrBlock value.
		self logFailure: failureString.
		TestResult failure sunitSignalWith: failureString]
			! !


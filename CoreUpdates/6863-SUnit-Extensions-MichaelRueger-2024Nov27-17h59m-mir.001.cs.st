'From Cuis7.1 [latest update: #6862] on 27 November 2024 at 6:13:57 pm'!

!TestCase methodsFor: 'testing' stamp: 'JF 7/30/2003 13:39'!
shouldPass
	"Unless the selector is in the list we get from #expectedFailures, we expect it to pass"
	^ (self expectedFailures includes: testSelector) not! !


!TestResult methodsFor: 'accessing' stamp: 'mir 11/21/2024 15:57:47'!
allFailures
	^ self unexpectedFailures, self unexpectedPasses ! !

!TestResult methodsFor: 'accessing' stamp: 'mir 11/21/2024 15:28:39'!
allPassed
	^ self expectedPasses, self expectedDefects! !

!TestResult methodsFor: 'accessing' stamp: 'JF 7/30/2003 13:54'!
expectedDefectCount
	^ self expectedDefects size! !

!TestResult methodsFor: 'accessing' stamp: 'md 11/25/2004 16:36'!
expectedDefects
	^ (errors, failures asOrderedCollection) select: [:each | each shouldPass not] ! !

!TestResult methodsFor: 'accessing' stamp: 'JF 7/30/2003 13:54'!
expectedPassCount
	^ self expectedPasses size! !

!TestResult methodsFor: 'accessing' stamp: 'JF 7/30/2003 16:14'!
expectedPasses
	^ passed select: [:each | each shouldPass] ! !

!TestResult methodsFor: 'accessing' stamp: 'JF 7/30/2003 13:54'!
unexpectedErrorCount
	^ self unexpectedErrors size! !

!TestResult methodsFor: 'accessing' stamp: 'JF 7/30/2003 16:14'!
unexpectedErrors
	^ errors select: [:each | each shouldPass] ! !

!TestResult methodsFor: 'accessing' stamp: 'JF 7/30/2003 13:54'!
unexpectedFailureCount
	^ self unexpectedFailures size! !

!TestResult methodsFor: 'accessing' stamp: 'JF 7/30/2003 16:14'!
unexpectedFailures
	^ failures select: [:each | each shouldPass] ! !

!TestResult methodsFor: 'accessing' stamp: 'JF 7/30/2003 13:54'!
unexpectedPassCount
	^ self unexpectedPasses size! !

!TestResult methodsFor: 'accessing' stamp: 'JF 7/30/2003 16:14'!
unexpectedPasses
	^ passed select: [:each | each shouldPass not] ! !


!TestCase methodsFor: 'running' stamp: 'mir 11/21/2024 15:52:57'!
run: aResult skipExpectedFailures: aBoolean
	ChangeSet
		runningTest: self printString
		do: [ aResult runCase: self ].! !


!TestResult methodsFor: 'printing' stamp: 'jmv 11/27/2024 18:12:40'!
printOn: aStream
	aStream
		nextPutAll: self runCount printString;
		nextPutAll: ' run, ';
		nextPutAll: self expectedPassCount printString;
		nextPutAll: ' passes, ';
		nextPutAll: self expectedDefectCount printString;
		nextPutAll:' expected failures/errors, ';
		nextPutAll: self unexpectedFailureCount printString;
		nextPutAll: ' failures, ';
		nextPutAll: self unexpectedErrorCount printString;
		nextPutAll:' errors, ';
		nextPutAll: self unexpectedPassCount printString;
		nextPutAll:' unexpected passes'.! !


'From Cuis 6.0 [latest update: #5652] on 24 January 2023 at 3:39:20 pm'!

!TestCase methodsFor: 'failing' stamp: 'jmv 1/24/2023 15:37:42'!
expectedFailures
	"Answer an array o selectors that are expected to fail.
	Usually this is done for tests that document incorrect behavior for which there is not yet a proper fix available."

	^#()! !

!TestCase methodsFor: 'running' stamp: 'jmv 1/24/2023 15:37:47'!
run: aResult skipExpectedFailures: aBoolean
	(aBoolean and: [self expectedFailures includes: testSelector])
		ifTrue: [
			Transcript show: 'Not ran (expected failure): '; print: self; newLine.
			^self ].
	ChangeSet
		runningTest: self printString
		do: [ aResult runCase: self ].! !


!TestCase methodsFor: 'running' stamp: 'jmv 1/24/2023 15:37:52'!
run
	| result |
	result := TestResult new.
	self run: result skipExpectedFailures: false.
	^result
			! !


!TestSuite methodsFor: 'running' stamp: 'jmv 1/24/2023 15:37:59'!
run: aResult

	| skipFailures t1 t2 |
	t1 _ Time localMillisecondClock.
	skipFailures _ self tests size > 1.
	self tests do: [:each | 
		self changed: each.
		each run: aResult skipExpectedFailures: skipFailures.
		"Allow for some responsivenes of the UI, or any other stuff that may need running"
		t2 _ Time localMillisecondClock.
		t2-t1 > 200 ifTrue: [
			Processor yield. 
			t1 _ t2 ]]! !

!methodRemoval: TestCase #run: stamp: 'jmv 1/24/2023 15:37:04'!
TestCase removeSelector: #run:!

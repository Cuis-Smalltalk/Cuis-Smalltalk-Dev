'From Cuis7.9 [latest update: #8215] on 24 September 2026 at 9:56:55 am'!

!TestResult methodsFor: 'printing' stamp: 'FJG 3/Sep/2026, 8:04:05 pm (UTC)'!
printOn: aStream
	aStream
		nextPutAll: self runCount printString;
		nextPutAll: ' run, ';
		nextPutAll: self expectedPassCount printString;
		nextPutAll: ' expected passes'.
	self expectedDefectCount > 0 ifTrue: [
		aStream
			nextPutAll: ', ';
			nextPutAll: self expectedDefectCount printString;
			nextPutAll: ' expected failures/errors'].
	self unexpectedFailureCount > 0 ifTrue: [
		aStream
			nextPutAll: ', ';
			nextPutAll: self unexpectedFailureCount printString;
			nextPutAll: ' unexpected failures'].
	self unexpectedErrorCount > 0 ifTrue: [
		aStream
			nextPutAll: ', ';
			nextPutAll: self unexpectedErrorCount printString;
			nextPutAll: ' unexpected errors'].
	self unexpectedPassCount > 0 ifTrue: [
		aStream
			nextPutAll: ', ';
			nextPutAll: self unexpectedPassCount printString;
			nextPutAll: ' unexpected passes']! !


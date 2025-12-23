'From Cuis7.7 [latest update: #7779] on 23 December 2025 at 4:39:11 pm'!
'Description '!
!provides: 'SUnitJsonReports' 1 1!
!requires: 'JSON' 1 nil nil!



!TestCase methodsFor: '*SUnitJsonReports' stamp: 'mir 20/Mar/2024 17:43:50'!
errorDescription
	^self failureDescription! !

!TestCase methodsFor: '*SUnitJsonReports' stamp: 'mir 20/Mar/2024 17:43:16'!
failureDescription
	^String streamContents: [:stream |
		self class printOn: stream.
		stream
			nextPutAll: '>>#';
			nextPutAll: (testSelector ifNil: [ 'unknown' ]).
		failureString ifNotNil: [
			stream
				nextPutAll: ': ';
				nextPutAll: failureString]]! !

!TestResult methodsFor: '*SUnitJsonReports' stamp: 'mir 20/Mar/2024 17:44:30'!
json
	| jsonDict |
	jsonDict := Dictionary new..
	jsonDict at: 'passed' put: 	(self passed collect: [:testCase | testCase printString]) asArray.

	jsonDict at: 'failures' put: 	(self failures collect: [:testCase | testCase failureDescription]) asArray.

	jsonDict at: 'errors' put: 	(self errors collect: [:testCase | testCase errorDescription]) asArray.

	jsonDict at:  'passCount' put: self correctCount.
	jsonDict at: 'failureCount ' put: self failureCount.
	jsonDict at: 'errorCount' put: self errorCount.
	^jsonDict asJsonString! !

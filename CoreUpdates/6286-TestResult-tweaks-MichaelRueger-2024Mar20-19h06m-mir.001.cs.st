'From Cuis6.3 [latest update: #6285] on 20 March 2024 at 7:09:18 pm'!

!TestResult methodsFor: 'printing' stamp: 'mir 3/20/2024 17:11:11'!
fullDescription
	" Return the description of the receiver. "

	| stream |
	stream := '' writeStream.
	stream lf; nextPutAll: 'passed:'; lf.
	self passed do: [:stage|
		stream lf;nextPutAll: '	',stage
	].
	stream lf; nextPutAll: 'failures:';lf.
	self failures do: [:testCase |
		stream lf;nextPutAll: '	',testCase selector ,'	' ,testCase failureString
	].
	stream lf; nextPutAll: 'errors:';lf.
	self errors do: [:testCase|
		stream lf;
			nextPutAll: '	',testCase selector ,'	' ,(testCase failureString ifNil: ['']); lf;
			yourself
	].
	stream lf; nextPutAll: 'passed: '; nextPutAll: self correctCount asString; lf.
	stream nextPutAll: 'failures: '; nextPutAll: self failureCount asString; lf.
	stream nextPutAll: 'errors: '; nextPutAll: self errorCount asString; lf.
	^stream contents! !


!TestResult methodsFor: 'printing' stamp: 'mir 3/20/2024 17:11:11'!
description
	" Return the description of the receiver. "

	| stream |
	stream := '' writeStream.
	stream lf; nextPutAll: 'failures:';lf.
	self failures do: [:testCase |
		stream lf;nextPutAll: '	',testCase selector ,'	' ,testCase failureString
	].
	stream lf; nextPutAll: 'errors:';lf.
	self errors do: [:testCase|
		stream lf;
			nextPutAll: '	',testCase selector ,'	' ,(testCase failureString ifNil: ['']); lf;
			yourself
	].
	stream lf; nextPutAll: 'passed: '; nextPutAll: self correctCount asString; lf.
	stream nextPutAll: 'failures: '; nextPutAll: self failureCount asString; lf.
	stream nextPutAll: 'errors: '; nextPutAll: self errorCount asString; lf.
	^stream contents! !


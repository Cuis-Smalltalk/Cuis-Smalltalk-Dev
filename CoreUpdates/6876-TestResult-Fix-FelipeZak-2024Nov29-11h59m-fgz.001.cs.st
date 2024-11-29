'From Cuis7.1 [latest update: #6875] on 29 November 2024 at 12:01:40 pm'!

!TestResult methodsFor: 'accessing' stamp: 'fgz 11/29/2024 11:59:31'!
unexpectedErrors
	^ errors ifNil: [ errors := OrderedCollection new ] ifNotNil: [ errors select: [:each | each shouldPass] ]! !

!TestResult methodsFor: 'accessing' stamp: 'fgz 11/29/2024 12:00:55'!
unexpectedFailures
	^ failures ifNil: [ failures := OrderedCollection new ] ifNotNil: [ failures select: [:each | each shouldPass] ]! !


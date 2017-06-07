'From Cuis 5.0 of 7 November 2016 [latest update: #3092] on 2 June 2017 at 11:21:59 am'!

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 11:01:27'!
defaultFailDescription

	^'Test failed'! !

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 11:02:09'!
fail

	^self failWith: self defaultFailDescription ! !

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 11:02:55'!
failWith: aDescription

	self signalFailure: aDescription ! !

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 10:57:13'!
should: aBlock raise: anExceptonHandlingCondition withExceptionDo: assertionsBlock description: aFailDescription

	^[aBlock value.
 	self failWith: aFailDescription ] 
		on: anExceptonHandlingCondition
		do: assertionsBlock ! !


!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 10:58:09'!
should: aBlock

	self assert: aBlock value
			! !

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 10:58:16'!
should: aBlock description: aString

	self assert: aBlock value description: aString
			! !

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 10:54:56'!
should: aBlock raise: anExceptonHandlingCondition 

	^self should: aBlock raise: anExceptonHandlingCondition withExceptionDo: [ :anException | ]
			! !

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 11:00:17'!
should: aBlock raise: anExceptonHandlingCondition description: aFailDescription

	^self should: aBlock raise: anExceptonHandlingCondition withExceptionDo: [:anException | ] description: aFailDescription! !

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 10:57:59'!
should: aBlock raise: anExceptonHandlingCondition withExceptionDo: assertionsBlock

	^self should: aBlock raise: anExceptonHandlingCondition withExceptionDo: assertionsBlock description: self defaultFailDescription! !

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 11:09:25'!
shouldnt: aBlock

	self deny: aBlock value
			! !

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 11:09:20'!
shouldnt: aBlock description: aString

	self deny: aBlock value description: aString
			! !

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 11:10:31'!
shouldnt: aBlock raise: anExceptonHandlingCondition 

	^self shouldnt: aBlock raise: anExceptonHandlingCondition description: anExceptonHandlingCondition printString, ' was not expected to be raised'! !

!TestCase methodsFor: 'Accessing' stamp: 'HAW 6/2/2017 11:06:04'!
shouldnt: aBlock raise: anExceptonHandlingCondition description: aFailDescription

	^aBlock 
		on: anExceptonHandlingCondition 
		do: [ :anException | self failWith: aFailDescription ]
! !

!methodRemoval: TestCase #executeShould:inScopeOf:!
TestCase removeSelector: #executeShould:inScopeOf:!
!methodRemoval: TestCase #executeShould:inScopeOf:withExceptionDo:!
TestCase removeSelector: #executeShould:inScopeOf:withExceptionDo:!

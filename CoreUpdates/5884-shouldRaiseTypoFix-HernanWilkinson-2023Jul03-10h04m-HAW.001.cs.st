'From Cuis 6.0 [latest update: #5883] on 3 July 2023 at 10:05:13 am'!

!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 11:36:48'!
should: aBlock raise: anExceptionHandlingCondition 

	self should: aBlock raise: anExceptionHandlingCondition withExceptionDo: [ :anException | ]
			! !

!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 11:36:34'!
should: aBlock raise: anExceptionHandlingCondition description: aFailDescription

	self should: aBlock raise: anExceptionHandlingCondition withExceptionDo: [:anException | ] description: aFailDescription! !

!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 11:36:39'!
should: aBlock raise: anExceptionHandlingCondition withExceptionDo: assertionsBlock

	self should: aBlock raise: anExceptionHandlingCondition withExceptionDo: assertionsBlock description: self defaultFailDescription! !

!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 11:37:05'!
shouldnt: aBlock raise: anExceptionHandlingCondition 

	self shouldnt: aBlock raise: anExceptionHandlingCondition description: anExceptionHandlingCondition printString, ' was not expected to be raised'! !

!TestCase methodsFor: 'assertions' stamp: 'sqr 6/26/2019 11:37:10'!
shouldnt: aBlock raise: anExceptionHandlingCondition description: aFailDescription

	aBlock 
		on: anExceptionHandlingCondition 
		do: [ :anException | self failWith: aFailDescription ]
! !


'From Cuis 5.0 of 7 November 2016 [latest update: #3285] on 5 April 2018 at 2:33:05 pm'!

!TestCase methodsFor: 'Accessing' stamp: 'HAW 4/5/2018 13:35:59'!
should: aBlock raise: anExceptionHandlingCondition withExceptionDo: assertionsBlock description: aFailDescription

	| result |
	
	[result := aBlock value ] 
		on: anExceptionHandlingCondition
		do: [ :anException |
			assertionsBlock value: anException.
			^result ].
		
	self failWith: aFailDescription! !


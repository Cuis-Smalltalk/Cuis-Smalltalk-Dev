'From Cuis 5.0 of 7 November 2016 [latest update: #3064] on 29 March 2017 at 6:09:56 pm'!

!FilterExceptionHandlingCondition methodsFor: 'handling condition creation - private' stamp: 'HAW 3/29/2017 17:59:55'!
createOrConditionWithExceptionType: anExceptionType

	^anExceptionType, handleCondition - filterCondition ! !


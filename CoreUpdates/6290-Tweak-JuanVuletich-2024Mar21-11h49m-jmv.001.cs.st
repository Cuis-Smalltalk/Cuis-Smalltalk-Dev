'From Cuis6.3 [latest update: #6289] on 21 March 2024 at 11:49:37 am'!

!TestCase methodsFor: 'assertions' stamp: 'jmv 3/20/2024 18:26:17'!
should: aBlockToFail raise: anExceptionHandlingCondition withMessageText: expectedErrorMessageOrBlock

	self 
		should: aBlockToFail 
		raise: anExceptionHandlingCondition 
		withExceptionDo: [ :anException |
			self assert: anException messageText equals: expectedErrorMessageOrBlock value ]! !


'From Cuis 4.0 of 21 April 2012 [latest update: #1495] on 4 December 2012 at 10:49:52 pm'!

!MessageNode methodsFor: 'printing' stamp: 'eem 11/5/2012 15:01'!
printWhileOn: aStream indent: level
	self printReceiver: receiver on: aStream indent: level.
	self
		printKeywords: originalSelector
		arguments: originalArguments
		on: aStream indent: level! !


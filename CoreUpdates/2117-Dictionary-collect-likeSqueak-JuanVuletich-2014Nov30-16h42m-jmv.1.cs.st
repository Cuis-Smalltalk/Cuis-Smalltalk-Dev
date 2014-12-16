'From Cuis 4.2 of 25 July 2013 [latest update: #2111] on 30 November 2014 at 6:46:24.98923 pm'!

!Dictionary methodsFor: 'enumerating' stamp: 'jmv 11/30/2014 18:46'!
collect: aBlock 
	"Evaluate aBlock with each of my values as the argument.  Collect the resulting values into a collection that is like me. Answer with the new collection."
	
	| newCollection |
	newCollection _ self species new: self size.
	self associationsDo: [ :each |
		newCollection at: each key put: (aBlock value: each value) ].
	^newCollection! !


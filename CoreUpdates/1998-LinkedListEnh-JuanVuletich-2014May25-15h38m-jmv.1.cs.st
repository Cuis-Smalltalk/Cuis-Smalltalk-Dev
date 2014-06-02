'From Cuis 4.2 of 25 July 2013 [latest update: #1996] on 25 May 2014 at 3:45:15.558987 pm'!

!LinkedList methodsFor: 'accessing' stamp: 'jmv 5/25/2014 15:44'!
at: index ifAbsent: exceptionBlock

	| i |
	index < 1 ifTrue: [ ^exceptionBlock value ].
	i _ 0.
	self do: [ :link |
		(i _ i + 1) = index ifTrue: [ ^ link ]].
	^ exceptionBlock value! !


!LinkedList methodsFor: 'accessing' stamp: 'jmv 5/25/2014 15:45'!
at: index

	^self at: index ifAbsent: [ self errorSubscriptBounds: index ]! !


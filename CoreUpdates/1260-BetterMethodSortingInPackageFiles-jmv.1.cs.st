'From Cuis 4.0 of 3 April 2012 [latest update: #1259] on 17 April 2012 at 11:13:57 pm'!

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 4/17/2012 23:13'!
sortedMethods
	^ self methods copy sort: [ :a :b | | aClassSymbol bClassSymbol aIsMeta bIsMeta |
		aClassSymbol _ a classSymbol.
		bClassSymbol _b classSymbol.
		aIsMeta _ a classIsMeta ifFalse: [ 0 ] ifTrue: [ 1 ].
		bIsMeta _ b classIsMeta ifFalse: [ 0 ] ifTrue: [ 1 ].
		aClassSymbol < bClassSymbol
			or: [ (aClassSymbol = bClassSymbol and: [ aIsMeta < bIsMeta ])
				or: [ aClassSymbol = bClassSymbol and: [ aIsMeta = bIsMeta and: [ a methodSymbol < b methodSymbol ]]]]]! !


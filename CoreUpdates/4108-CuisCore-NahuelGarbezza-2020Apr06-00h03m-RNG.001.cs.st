'From Cuis 5.0 [latest update: #4103] on 6 April 2020 at 12:05:43 am'!
"Change Set:		4104-CuisCore-AuthorName-2020Apr06-00h03m
Date:			6 April 2020
Author:			Nahuel Garbezza

Change Symbol>>isValidSelector implementation to support binary messages"!


!Symbol methodsFor: 'testing' stamp: 'RNG 3/29/2020 19:56:14'!
isValidBinarySelector

	^ self isInfix and: [ self allSatisfy: [ :character | character isValidInBinarySelectors ] ]! !

!Symbol methodsFor: 'testing' stamp: 'RNG 3/29/2020 19:56:38'!
isValidKeywordSelector

	^ self isKeyword and: [ self keywords allSatisfy: [ :keywordString | keywordString allButLast asSymbol isValidSelector ] ]! !

!Symbol methodsFor: 'testing' stamp: 'RNG 3/29/2020 19:56:03'!
isValidUnarySelector

	^ self isUnary and: [ self allSatisfy: [ :character | character isValidInIdentifiers ] ]! !


!Symbol methodsFor: 'testing' stamp: 'RNG 3/29/2020 19:56:45'!
isValidSelector

	^ self isValidUnarySelector
		or: [ self isValidBinarySelector ]
		or: [ self isValidKeywordSelector ]! !


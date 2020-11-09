'From Cuis 5.0 [latest update: #4438] on 6 November 2020 at 9:58:08 am'!

!PseudoClass methodsFor: 'methods' stamp: 'jmv 11/6/2020 09:57:50'!
removeSelector: aSelector
	| catName |
	catName := self class removedCategoryName asString.
	self organization addCategory: catName before: self organization categories first.
	self organization classify: aSelector under: catName.
	self sourceCodeAt: aSelector put:'methodWasRemoved' asText.! !


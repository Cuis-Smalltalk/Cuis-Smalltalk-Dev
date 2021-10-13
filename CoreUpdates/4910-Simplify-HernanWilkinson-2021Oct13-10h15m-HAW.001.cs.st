'From Cuis 5.0 [latest update: #4909] on 13 October 2021 at 10:16:22 am'!

!Categorizer methodsFor: 'tdd' stamp: 'HAW 2/7/2017 09:06:13'!
classesAt: aCategoryName

	^(self listAtCategoryNamed: aCategoryName) collect: [:aClassName | Smalltalk classNamed: aClassName ]
		! !


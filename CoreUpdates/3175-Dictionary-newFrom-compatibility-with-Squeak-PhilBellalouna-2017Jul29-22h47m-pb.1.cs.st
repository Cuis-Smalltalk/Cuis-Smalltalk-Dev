'From Cuis 5.0 of 7 November 2016 [latest update: #3138] on 29 July 2017 at 10:49:50 pm'!

!Dictionary class methodsFor: 'instance creation' stamp: 'pb 7/29/2017 22:47:17'!
newFrom: aDict 
	"Answer an instance of me containing the same associations as aDict.
	 Error if any key appears twice."
	| newDictionary |
	newDictionary _ self new: aDict size.
	aDict associationsDo:
		[:x |
		(newDictionary includesKey: x key)
			ifTrue: [self error: 'Duplicate key: ', x key printString]
			ifFalse: [newDictionary add: x copy]].
	^ newDictionary

"	NewDictionary newFrom: {1->#a. 2->#b. 3->#c}
	{1->#a. 2->#b. 3->#c} as: NewDictionary
	NewDictionary newFrom: {1->#a. 2->#b. 1->#c}
	{1->#a. 2->#b. 1->#c} as: NewDictionary
"! !


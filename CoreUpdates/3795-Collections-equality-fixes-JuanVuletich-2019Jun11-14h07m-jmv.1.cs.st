'From Cuis 5.0 of 7 November 2016 [latest update: #3793] on 11 June 2019 at 2:08:54 pm'!

!Semaphore methodsFor: 'private' stamp: 'jmv 6/11/2019 12:31:53'!
species
	"So we are never equal an Array"

	^ self class! !


!RunArray methodsFor: 'accessing' stamp: 'jmv 6/11/2019 12:34:02'!
= otherArray 
	self == otherArray ifTrue: [ ^ true ].
	
	self species == otherArray species ifFalse: [^ false].

	"Test if all my elements are equal to those of otherArray"
	(otherArray isMemberOf: RunArray) ifFalse: [^ self hasEqualElements: otherArray].

	"Faster test between two RunArrays"
 	^ (runs hasEqualElements: otherArray runs)
		and: [values hasEqualElements: otherArray values]! !


!Dictionary methodsFor: 'testing' stamp: 'jmv 6/11/2019 12:41:01'!
is: aSymbol
	"Dictionaries and Sets have different #species. So, aDictionary is: #Set should be false."
	aSymbol == #Set ifTrue: [ ^ false ].
	^aSymbol == #Dictionary or: [ super is: aSymbol ]! !


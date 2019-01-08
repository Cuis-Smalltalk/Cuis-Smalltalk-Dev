'From Cuis 5.0 of 7 November 2016 [latest update: #3562] on 7 January 2019 at 10:46:01 pm'!

!ClassBuilder class methodsFor: 'accessing' stamp: 'HAW 1/7/2019 22:32:41'!
reservedNames

	"Return a list of names that must not be used for variables"

	^#(#self #super #true #false #nil #thisContext)! !


!ClassBuilder methodsFor: 'private' stamp: 'HAW 1/7/2019 22:32:29'!
reservedNames

	"Return a list of names that must not be used for variables"

	^self class reservedNames ! !


!Theme methodsFor: 'private - shout mappings' stamp: 'HAW 1/7/2019 22:33:06'!
pseudoVariables

	^ ClassBuilder reservedNames ! !


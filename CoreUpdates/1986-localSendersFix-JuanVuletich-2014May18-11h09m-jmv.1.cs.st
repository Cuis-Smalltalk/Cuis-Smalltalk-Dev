'From Cuis 4.2 of 25 July 2013 [latest update: #1982] on 18 May 2014 at 11:11:03.782284 am'!

!Behavior methodsFor: 'user interface' stamp: 'jmv 5/18/2014 11:09'!
allLocalCallsOn: aSymbol
	"Answer a SortedCollection of all the methods that call on aSymbol, anywhere in my class hierarchy."

	| aSet special byte cls |
	aSet _ Set new.
	cls _ self theNonMetaClass.
	special _ Smalltalk 
		hasSpecialSelector: aSymbol
		ifTrueSetByte: [ :b | byte _ b ].
	cls withAllSuperAndSubclassesDoGently: [ :class |
		(class whichSelectorsReferTo: aSymbol special: special byte: byte)
			do: [ :sel | aSet add: class name , ' ', sel ]].
	cls class withAllSuperAndSubclassesDoGently: [ :class |
		(class whichSelectorsReferTo: aSymbol special: special byte: byte)
			do: [ :sel | aSet add: class name , ' ', sel]].
	^aSet! !


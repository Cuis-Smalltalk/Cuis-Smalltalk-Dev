'From Cuis 5.0 of 7 November 2016 [latest update: #3092] on 2 June 2017 at 4:06:08 pm'!

!Behavior methodsFor: 'user interface' stamp: 'HAW 6/2/2017 16:05:24'!
addTo: aSet referencesTo: aSymbol special: special byte: byte.
	
	self withAllSuperAndSubclassesDoGently: [ :class |
		(class whichSelectorsReferTo: aSymbol special: special byte: byte)
			do: [ :sel | aSet add: (MethodReference class: class selector: sel) ]].
	! !


!Behavior methodsFor: 'user interface' stamp: 'HAW 6/2/2017 16:05:35'!
allLocalCallsOn: aSymbol
	"Answer a SortedCollection of all the methods that call on aSymbol, anywhere in my class hierarchy."

	| aSet special byte cls |
	
	aSet _ Set new.
	cls _ self theNonMetaClass.
	special _ Smalltalk 
		hasSpecialSelector: aSymbol
		ifTrueSetByte: [ :b | byte _ b ].
	
	cls addTo: aSet referencesTo: aSymbol special: special byte: byte.
	cls class addTo: aSet referencesTo: aSymbol special: special byte: byte.
	
	^aSet! !


'From Cuis 6.0 [latest update: #5216] on 3 June 2022 at 9:35:17 am'!

!ArrayOfCharactersAndCodePoints methodsFor: 'testing' stamp: 'jmv 6/1/2022 11:29:22'!
is: aSymbol
	^ aSymbol == #ArrayOfCharactersAndCodePoints or: [ super is: aSymbol ]! !


!Utf32String methodsFor: 'testing' stamp: 'jmv 6/1/2022 11:32:13'!
canHoldCodePoints

	^true! !


!Utf8String methodsFor: 'testing' stamp: 'jmv 6/1/2022 11:32:02'!
canHoldCodePoints

	^true! !


!WriteStream methodsFor: 'private' stamp: 'jmv 6/1/2022 11:29:56'!
isCompatibleWithContents: aCollection

	collection class == aCollection class
		ifTrue: [ ^ true ].

	(aCollection isString and: [ (collection is: #Text) or: [ collection is: #ArrayOfCharactersAndCodePoints ]])
		ifTrue: [ ^ true ].

	^ false! !


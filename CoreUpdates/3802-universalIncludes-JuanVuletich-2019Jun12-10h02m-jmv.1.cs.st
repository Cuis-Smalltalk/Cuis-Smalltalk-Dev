'From Cuis 5.0 of 7 November 2016 [latest update: #3799] on 12 June 2019 at 10:15:34 am'!

!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 6/12/2019 10:11:23'!
includes: aCharacter
	(aCharacter is: #Character) ifFalse: [ ^ false ].
	^(map at: aCharacter numericValue + 1) > 0! !


!Interval methodsFor: 'testing' stamp: 'jmv 6/12/2019 10:14:04'!
includes: aNumber
	aNumber isNumber ifFalse: [ ^ false ].
	^ aNumber between: self first and: self last! !


!Trie methodsFor: 'testing' stamp: 'jmv 6/12/2019 10:14:56'!
includes: aString
	"Consistent with Set, but not with Dictionary,  as in Dictionary, #includes:
	finds a value regardless of the key. To get this behavior, use #includesValue:"

	aString isString ifFalse: [ ^ false ].
	self at: aString ifPresent: [ :v | ^v = aString ].
	^false! !


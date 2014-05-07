'From Cuis 4.2 of 25 July 2013 [latest update: #1981] on 5 May 2014 at 10:56:45.074131 pm'!

!Text methodsFor: 'emphasis' stamp: 'jmv 5/5/2014 22:34'!
initialFont: aFont
	"Apply aFont to those parts that are not already specifying one.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	| fontAttr |
	fontAttr _ TextFontFamilyAndSize
			familyName: aFont familyName
			pointSize: aFont pointSize.
	runs mapValues: [ :attributes |
		(attributes anySatisfy: [ :attr | attr isFont ])
			ifTrue: [ attributes ]
			ifFalse: [ attributes copyWith: fontAttr ]]! !


!TextAttribute methodsFor: 'testing' stamp: 'jmv 5/5/2014 22:32'!
isFont
	^false! !


!TextFontFamilyAndSize methodsFor: 'testing' stamp: 'jmv 5/5/2014 22:32'!
isFont
	^true! !


!Text class methodsFor: 'instance creation' stamp: 'jmv 5/5/2014 22:56'!
initialFont: aFont stringOrText: aStringOrText
	"Answer an instance of me whose characters are aString.
	Note: use #baseFont. If emphasis is desired, add it separatedly."

	(aStringOrText is: #Text) ifTrue: [
		^aStringOrText initialFont: aFont ].
	
	^self string: aStringOrText attributes: {TextFontFamilyAndSize
		familyName: aFont familyName
		pointSize: aFont pointSize }! !


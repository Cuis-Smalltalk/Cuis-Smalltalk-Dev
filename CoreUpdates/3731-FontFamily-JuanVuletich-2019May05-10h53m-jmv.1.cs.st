'From Cuis 5.0 of 7 November 2016 [latest update: #3725] on 5 May 2019 at 10:54:32 am'!
!classDefinition: #FontFamily category: #'Graphics-Text'!
Object subclass: #FontFamily
	instanceVariableNames: 'familyName baseFontBySizes'
	classVariableNames: 'AvailableFamilies DefaultFamilyName DefaultPointSize'
	poolDictionaries: ''
	category: 'Graphics-Text'!

!FontFamily commentStamp: '<historical>' prior: 0!
Also called Typeface.!

!classDefinition: #StrikeFontFamily category: #'Graphics-Text'!
FontFamily subclass: #StrikeFontFamily
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!

!FontFamily methodsFor: 'initialization' stamp: 'jmv 5/5/2019 10:33:56'!
atPointSize: aNumber
	^baseFontBySizes at: aNumber ifAbsent: nil! !

!FontFamily methodsFor: 'initialization' stamp: 'jmv 5/4/2019 16:11:02'!
atPointSize: aNumber put: aFontAndSize
	"aFontAndSize must have emphasis = 0, i.e. it is a base font"
	baseFontBySizes at: aNumber put: aFontAndSize ! !

!FontFamily methodsFor: 'initialization' stamp: 'jmv 5/4/2019 16:00:18'!
familyName: aString
	familyName _ aString.
	baseFontBySizes _ Dictionary new! !

!FontFamily methodsFor: 'accessing' stamp: 'jmv 5/4/2019 16:09:26'!
familyName
	^ familyName! !


!FontFamily class methodsFor: 'accessing' stamp: 'jmv 5/5/2019 10:54:03'!
familyNames
	"
	FontFamily familyNames
	"
	^AvailableFamilies keys sort! !

!FontFamily class methodsFor: 'initialization' stamp: 'jmv 5/5/2019 10:54:06'!
initialize
	"
	FontFamily initialize
	"
	(AvailableFamilies isNil and: [AbstractFont availableFonts notNil]) ifTrue: [
		self migrate ]! !

!FontFamily class methodsFor: 'initialization' stamp: 'jmv 5/5/2019 10:54:12'!
migrate
	"
	FontFamily migrate
	"
	| family def strikeFontAndSize |
	AvailableFamilies _ Dictionary new.
	def _ AbstractFont default.

	AbstractFont familyNames do: [ :familyName |
		family _ StrikeFontFamily new.
		family familyName: familyName.
		(AbstractFont pointSizesFor: familyName) do: [ :ps |
			strikeFontAndSize _ AbstractFont familyName: familyName pointSize: ps.
			family atPointSize: ps put: strikeFontAndSize.
			def == strikeFontAndSize ifTrue: [
				DefaultFamilyName _ family familyName.
				DefaultPointSize _ ps ]].
		AvailableFamilies at: family familyName put: family ].! !

!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 5/5/2019 10:54:00'!
defaultFamilyPointSize: aNumber
	"
	FontFamily defaultFamilyPointSize: 12
	"
	| family |
	family _ AvailableFamilies at: DefaultFamilyName.
	^family atPointSize: aNumber! !

!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 5/5/2019 10:54:08'!
familyName: aString pointSize: aNumber
	"
	FontFamily familyName: 'DejaVu' pointSize: 12
	"
	| family |
	family _ AvailableFamilies at: aString ifAbsent: [^nil].
	^family atPointSize: aNumber! !

FontFamily initialize!

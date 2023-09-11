'From Cuis 6.0 [latest update: #6024] on 11 September 2023 at 7:48:39 pm'!

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 9/11/2023 19:47:47'!
readTrueTypeFontEntry: ttFontFileEntry

	| bytes tag ttFontDescriptions familyName ttFontFamily 
	read thisFamilyDict |
	bytes := ttFontFileEntry binaryContents.
	tag := bytes copyFrom: 1 to: 4.
	tag = `'ttcf' asUtf8Bytes`
		ifTrue: [ ttFontDescriptions := TTFontReader readTTCFrom: bytes ]
		ifFalse: [ ttFontDescriptions := {TTFontReader readTTFFrom: bytes} ].

	"Separate in different families if needed. See #add:to:"	
	read := OrderedDictionary new.
	ttFontDescriptions do: [ :ttFontDescription |
		thisFamilyDict := read at: ttFontDescription familyName ifAbsentPut: [ Dictionary new].
		(thisFamilyDict includesKey: ttFontDescription emphasis) ifTrue: [
			ttFontDescription familyName: ttFontDescription familyName, '-', ttFontDescription subfamilyName subFamilyName: ''.
			thisFamilyDict := read at: ttFontDescription familyName ifAbsentPut: [ Dictionary new]].
		thisFamilyDict at: ttFontDescription emphasis put: ttFontDescription ].

	ttFontDescriptions do: [ :ttFontDescription |
		familyName := ttFontDescription familyName.
		ttFontFamily := FontFamily familyNamed: familyName.
		ttFontFamily
			ifNil: [ FontFamily addFamily: (self new baseTTFontDescription: ttFontDescription ) ]
			ifNotNil: [ :fam |
				ttFontDescription emphasis = 0
					ifTrue: [fam baseTTFontDescription: ttFontDescription]
					ifFalse: [fam addTTFontDescription: ttFontDescription].
				fam clearCache ].
		familyName = FontFamily defaultFamilyName ifTrue: [
			PreferenceSet setDefaultFont:	 familyName ].
		('loaded font ', familyName) print ].! !


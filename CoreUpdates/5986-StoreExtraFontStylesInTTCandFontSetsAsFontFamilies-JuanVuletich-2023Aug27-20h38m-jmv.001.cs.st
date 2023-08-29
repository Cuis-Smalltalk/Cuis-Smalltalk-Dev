'From Cuis 6.0 [latest update: #5983] on 27 August 2023 at 8:40:25 pm'!

!TTFontDescription methodsFor: 'private-initialization' stamp: 'jmv 8/27/2023 20:39:23'!
familyName: aString subFamilyName: anotherString
	familyName := aString.
	subfamilyName := anotherString.! !


!TTFontDescription class methodsFor: 'instance creation' stamp: 'jmv 8/27/2023 20:39:26'!
add: ttf to: aDictionary
	"Add a TTFontDesctiption to aDictionary.
	If an emphasis is already found, then come up with another family name.
	In Cuis, each font family can have up to 4 fonts: Regular, Bold, Italic, Bold Italic.
	But some font families include many variations. Install them as different families.
	See senders."

	| thisFamilyDict |
	thisFamilyDict := aDictionary at: ttf familyName ifAbsentPut: [ Dictionary new].
	(thisFamilyDict includesKey: ttf emphasis) ifTrue: [
		ttf familyName: ttf familyName, '-', ttf subfamilyName subFamilyName: ''.
		thisFamilyDict := aDictionary at: ttf familyName ifAbsentPut: [ Dictionary new]].
	thisFamilyDict at: ttf emphasis put: ttf.! !


!FontFamily class methodsFor: 'ui' stamp: 'jmv 8/27/2023 20:39:30'!
readIfNeeded: nameOrDirectory
	"Read from disk if not in memory"
	(FontFamily familyNamed: nameOrDirectory)
		ifNotNil: [ :it | ^it ].
	^FontFamily availableFamilies values
		detect: [ :any | any folderName = nameOrDirectory name ]
		ifNone: [
			FontFamily familyNamed: (TrueTypeFontFamily
				readAllTrueTypeFontsIn: nameOrDirectory) first ]! !


!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 8/27/2023 20:39:33'!
readAdditionalFonts

	TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'! !

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 8/27/2023 20:39:36'!
readAllTrueTypeFontsIn: aDirectoryEntry
	"
	TrueTypeFontFamily readAllTrueTypeFontsIn: '/home/juan/Downloads/DejaVu2.37/dejavu-fonts-ttf-2.37/ttf/' asDirectoryEntry
	"
	| readFamilyNames |
	readFamilyNames := TTFontDescription readTrueTypeFontsIn: aDirectoryEntry.
	readFamilyNames do: [ :familyName |
		| ttDescriptions baseTTDescription family |
		ttDescriptions := TTFontDescription withFamilyName: familyName.
		baseTTDescription := ttDescriptions at: 0.
		family := TrueTypeFontFamily baseTTFontDescription: baseTTDescription.
		ttDescriptions do: [ :ttfDesc |
			ttfDesc emphasis = 0 ifFalse: [
				family addTTFontDescription: ttfDesc ]].
		FontFamily addFamily: family ].
	^readFamilyNames! !


!TTFontDescription class methodsFor: 'instance creation' stamp: 'jmv 8/27/2023 20:39:41'!
readTrueTypeFontsIn: aDirectoryEntry
	"
	TTFontDescription read: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'
	"
	| read |
	read := OrderedDictionary new.
	aDirectoryEntry
		allFilesDo: [ :file | | ttf |
			ttf := TTFontReader readTTFFrom: file binaryContents.
			ttf folderName: file parent name.
			self add: ttf to: read ]
		matches: [ :file | '*.ttf' match: file name ].
	aDirectoryEntry
		allFilesDo: [ :file | | ttc |
			ttc := TTFontReader readTTCFrom: file binaryContents.
			ttc do: [ :eachTtf |
				eachTtf folderName: file parent name.
				self add: eachTtf to: read ]]
		matches: [ :file | '*.ttc' match: file name ].
	Descriptions ifNil: [ Descriptions := Dictionary new ].
	read keysAndValuesDo: [ :familyName :emphasisDict |
		emphasisDict keysAndValuesDo: [ :emphasis :ttf |
			(Descriptions at: familyName ifAbsentPut: [ Dictionary new]) at: emphasis put: ttf ]].
	^read keys.! !


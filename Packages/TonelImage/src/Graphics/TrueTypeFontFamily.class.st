"
Also called Typeface. See https://en.wikipedia.org/wiki/Typeface
Also see AbstractFont / TrueTypeFont
Also see TTFontDescription

My instances have a base TTFontDescription (such as DejaVu Sans Book) and (if available) a dictionary of TTFontDescription by emphasis (such as DejaVu Sans Oblique and DejaVu Sans Bold).

They also have a dictionary of base TrueTypeFont instances, indexed by pointSize, which in turn know their derivative for various emphases.
"
Class {
	#name : 'TrueTypeFontFamily',
	#superclass : 'FontFamily',
	#instVars : [
		'baseTTFontDescription',
		'ttFontDescriptionsByEmphasis'
	],
	#category : 'Graphics-TrueType',
	#package : 'Graphics',
	#tag : 'TrueType'
}

{ #category : 'instance creation' }
TrueTypeFontFamily class >> baseTTFontDescription: aTTFontDescription [
	^ self new baseTTFontDescription: aTTFontDescription
]

{ #category : 'instance creation' }
TrueTypeFontFamily class >> fileReaderServicesForFileEntry: filename suffix: suffix [
	"FileList buttons that read a TrueType font on contents."

	(#( 'ttf' 'ttc') includes: suffix) ifTrue: [
		^ { self serviceReadTTFont } ].

	^#()
]

{ #category : 'instance creation' }
TrueTypeFontFamily class >> readAdditionalFonts [

	TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory.
]

{ #category : 'instance creation' }
TrueTypeFontFamily class >> readAllTrueTypeFontsIn: aDirectoryEntry [
	"
	TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory / 'DejaVu' / 'DejaVuSans'
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
	^readFamilyNames
]

{ #category : 'instance creation' }
TrueTypeFontFamily class >> readTrueTypeFontEntry: ttFontFileEntry [

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
		('loaded font ', familyName) print ].
]

{ #category : 'instance creation' }
TrueTypeFontFamily class >> serviceReadTTFont [
	"Answer the service of reading a TrueType font"

	^ (SimpleServiceEntry
		provider: self 
		label: 'load font'
		selector: #readTrueTypeFontEntry:
		description: 'load TrueType font'
		buttonLabel: 'read font'
		icon: #fontIcon
		) argumentGetter: [ :fileList | fileList selectedFileEntry ]
]

{ #category : 'initialization' }
TrueTypeFontFamily >> addTTFontDescription: aTTFontDescription [
	ttFontDescriptionsByEmphasis at: aTTFontDescription emphasis put: aTTFontDescription.
]

{ #category : 'accessing' }
TrueTypeFontFamily >> atPointSize: aNumber [
	| baseFont |
	baseFontBySizes at: aNumber ifPresent: [ :existingBaseFont | ^ existingBaseFont ].
	baseFont := TrueTypeFont family: self ttFontDescription: baseTTFontDescription pointSize: aNumber.
	baseFontBySizes at: aNumber put: baseFont.
	ttFontDescriptionsByEmphasis do: [ :emphasizedTTFontDescription | | emphasizedFont |
		emphasizedFont := TrueTypeFont family: self ttFontDescription: emphasizedTTFontDescription pointSize: aNumber.
		emphasizedFont baseFont: baseFont.
		baseFont addDerivativeFont: emphasizedFont ].
	^ baseFont
]

{ #category : 'initialization' }
TrueTypeFontFamily >> baseTTFontDescription: aTTFontDescription [
	self assert: aTTFontDescription emphasis = 0.
	familyName := aTTFontDescription familyName.
	baseFontBySizes := Dictionary new.
	baseTTFontDescription := aTTFontDescription.
	ttFontDescriptionsByEmphasis isNil ifTrue: [ ttFontDescriptionsByEmphasis := Dictionary new ].
]

{ #category : 'initialization' }
TrueTypeFontFamily >> clearCache [
	"
	TrueTypeFontFamily  allInstancesDo: [ :each | each clearCache ].
	"
	| emphasized |
	emphasized := ttFontDescriptionsByEmphasis values.
	self baseTTFontDescription: baseTTFontDescription.
	emphasized do: [ :each |
		self addTTFontDescription: each ]
]

{ #category : 'accessing' }
TrueTypeFontFamily >> folderName [
	^baseTTFontDescription folderName
]

{ #category : 'testing' }
TrueTypeFontFamily >> includesAscii [
	"Answer true if we include the basic ASCII characters.
	If not, we are not suitable, for instance, for displaying Smalltalk code."
	^baseTTFontDescription includesAscii
]

{ #category : 'testing' }
TrueTypeFontFamily >> isTrueTypeFontFamily [
	^true
]

{ #category : 'accessing' }
TrueTypeFontFamily >> pointSizes [
	^#(6 7 8 9 10 11 12 13 14 15 18 22 26 36 48 72 96 144)
]

"
A FontFamily is a collection of TrueType fonts that belong together and share a font name.
For example, Cuis ships with the DejaVu Sans font family.

Also called Typeface. See https://en.wikipedia.org/wiki/Typeface

See AbstractFont / TrueTypeFont.
See TTFontDescription.
"
Class {
	#name : 'FontFamily',
	#superclass : 'Object',
	#instVars : [
		'familyName',
		'baseFontBySizes'
	],
	#classVars : [
		'AvailableFamilies',
		'DefaultFamilyName',
		'DefaultPointSize'
	],
	#category : 'Graphics-Text',
	#package : 'Graphics',
	#tag : 'Text'
}

{ #category : 'initialization' }
FontFamily class >> addFamily: aFontFamily [
	AvailableFamilies at: aFontFamily familyName put: aFontFamily
]

{ #category : 'ui' }
FontFamily class >> addFontDir: dir to: menu current: currentNameOrNil [
	| fontNameFile fontName |
	fontNameFile := dir // 'fontName.txt'.
	fontName := fontNameFile exists
		ifTrue: [ fontNameFile textContents ]
		ifFalse: [ dir name ].

	(FontFamily availableFamilies includesKey: fontName) ifTrue: [
		^false ].

	menu
		add: (fontName=currentNameOrNil ifTrue: ['<on>'] ifFalse: ['<off>']), fontName
		action: #modalSelection:
		argument: {#fontDirectory. fontName. dir}.
	^true
]

{ #category : 'accessing' }
FontFamily class >> availableFamilies [
	^AvailableFamilies
]

{ #category : 'accessing fonts' }
FontFamily class >> defaultFamilyAndPointSize [
	"
	FontFamily defaultFamilyAndPointSize
	"
	| family |
	family := AvailableFamilies at: DefaultFamilyName.
	^family atPointSize: DefaultPointSize 
]

{ #category : 'accessing' }
FontFamily class >> defaultFamilyName [
	^ DefaultFamilyName
]

{ #category : 'initialization' }
FontFamily class >> defaultFamilyName: aString [
	self defaultFamilyName: aString defaultPointSize: nil
]

{ #category : 'initialization' }
FontFamily class >> defaultFamilyName: aStringOrNil defaultPointSize: aNumberOrNil [
	"Nil just means leave it as it is now."

	aStringOrNil ifNotNil: [DefaultFamilyName := aStringOrNil].
	aNumberOrNil ifNotNil: [DefaultPointSize := aNumberOrNil].
]

{ #category : 'accessing fonts' }
FontFamily class >> defaultFamilyPointSize: aNumber [
	"
	FontFamily defaultFamilyPointSize: 12
	"
	| family |
	family := AvailableFamilies at: DefaultFamilyName.
	^family atPointSize: aNumber
]

{ #category : 'defaults' }
FontFamily class >> defaultLineSpacing [
	^FontFamily defaultFamilyAndPointSize lineSpacing
]

{ #category : 'accessing' }
FontFamily class >> defaultPointSize [
	"
	FontFamily defaultPointSize
	"
	^ DefaultPointSize 
]

{ #category : 'initialization' }
FontFamily class >> defaultPointSize: aNumber [
	self defaultFamilyName: nil defaultPointSize: aNumber
]

{ #category : 'accessing fonts' }
FontFamily class >> familyName: aString pointSize: aNumber [
	"
	FontFamily familyName: FontFamily defaultFamilyName pointSize: 12
	FontFamily defaultFamilyPointSize: 12
	"
	| family |
	family := AvailableFamilies at: aString ifAbsent: [^nil].
	^family atPointSize: aNumber
]

{ #category : 'accessing fonts' }
FontFamily class >> familyNamed: aString [
	"
	FontFamily familyNamed: FontFamily defaultFamilyName
	"
	^AvailableFamilies at: aString ifAbsent: nil.
]

{ #category : 'accessing' }
FontFamily class >> familyNames [
	"
	FontFamily familyNames
	"
	^AvailableFamilies keys sort
]

{ #category : 'accessing fonts' }
FontFamily class >> pointSizesFor: aString [
	"
	FontFamily pointSizesFor: FontFamily defaultFamilyName
	"
	| family |
	family := AvailableFamilies at: aString ifAbsent: [^nil].
	^family pointSizes
]

{ #category : 'ui' }
FontFamily class >> promptUser [
	"Present a menu of font families, answer selection.
	FontFamily promptUser
	"
	^self promptUser: FontFamily defaultFamilyName includeNonASCII: true
]

{ #category : 'ui' }
FontFamily class >> promptUser: currentNameOrNil [
	"
	FontFamily promptUser
	FontFamily promptUser: FontFamily defaultFamilyName
	"
	"Present a menu of available fonts, and if one is chosen, return it.
	Otherwise return nil."

	^self promptUser: currentNameOrNil includeNonASCII: true
]

{ #category : 'ui' }
FontFamily class >> promptUser: currentNameOrNil includeNonASCII: showFontsNotIncludingASCIICharacters [
	"
	FontFamily promptUser
	FontFamily promptUser: FontFamily defaultFamilyName
	"
	"Present a menu of available fonts, and if one is chosen, return it.
	Otherwise return nil."

	| menu answer |
	menu := MenuMorph entitled: 'Fonts'.
	menu defaultTarget: menu.
	menu addTitle: 'Installed Fonts'.
	FontFamily familyNames do: [ :fontName | | fontFamily item |
		fontFamily := FontFamily familyNamed: fontName.
		(fontFamily includesAscii or: [showFontsNotIncludingASCIICharacters]) ifTrue: [
			item := menu
				add: (fontName = currentNameOrNil) asMenuItemTextPrefix, fontName
				target: menu
				action: #modalSelection:
				argument: {#fontName. fontName}.
			fontFamily includesAscii ifTrue: [
				item fontFamily: fontFamily ]]].

	menu addTitle: 'Available to install'.
	(DirectoryEntry trueTypeFontsDirectory
		directories sorted: [ :a :b | a name < b name ]) do: [ :dir | | subDirs |
			subDirs := dir directories sorted: [ :a :b | a name < b name ].
			subDirs isEmpty
				ifTrue: [
					self addFontDir: dir to: menu current: currentNameOrNil ]
				ifFalse: [ | submenu anyAdded |
					submenu := MenuMorph new defaultTarget: menu.
					anyAdded := false.
					subDirs do: [ :subdir |
						anyAdded := anyAdded | (self addFontDir: subdir to: submenu current: currentNameOrNil) ].
					anyAdded ifTrue: [
						menu add: dir name, '... ' subMenu: submenu ]]].
	"If there's nothing 'Available to install', remove the section title"
	(menu lastSubmorph is: #MenuItemMorph) ifFalse: [
		menu lastSubmorph delete ].

	answer := menu invokeModal.
	answer ifNil: [ ^nil ].
	answer first = #fontDirectory
		ifTrue: [ TrueTypeFontFamily readAllTrueTypeFontsIn: answer third ].
	^FontFamily familyNamed: answer second
]

{ #category : 'ui' }
FontFamily class >> promptUserAlt [
	"Present a menu of font families, answer selection.
	Alternative implementation: Show only installed fonts.
	FontFamily promptUserAlt
	"
	| fontMenu priorFontFamily |
	priorFontFamily := FontFamily defaultFamilyName.
	fontMenu := MenuMorph entitled: 'Installed Fonts'.
	FontFamily familyNames do: [ :fontName | | fontFamily item |
		item := fontMenu
			add: (priorFontFamily sameAs: fontName) asMenuItemTextPrefix, fontName
			target: fontMenu
			action: #modalSelection:
			argument: fontName.
		fontFamily := FontFamily familyNamed: fontName.
		fontFamily includesAscii ifTrue: [
			item fontFamily: fontFamily ]].
	^FontFamily familyNamed: fontMenu invokeModal.
]

{ #category : 'ui' }
FontFamily class >> promptUserAndSetDefault [
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	(self promptUser: FontFamily defaultFamilyName includeNonASCII: false) ifNotNil: [ :fontFamily |
		fontFamily includesAscii
			ifTrue: [PreferenceSet setDefaultFont: fontFamily familyName ]
			ifFalse: [self inform: 'Selected font does not include ASCII characters. Can not be set as default.' ]].
]

{ #category : 'ui' }
FontFamily class >> promptUserForSize: currentSize fontFamily: fontFamily [
	| menu |
	menu := MenuMorph entitled: 'Font Sizes'.
	menu defaultTarget: menu.
	fontFamily pointSizes do: [ :pt | | itemText sizeToUse |
		itemText := pt printString.
		sizeToUse := pt.
		(menu
			add: (pt = currentSize) asMenuItemTextPrefix, itemText
			action: #modalSelection:
			argument: pt)
				font: (fontFamily atPointSize: sizeToUse) ].
	^ menu invokeModal
]

{ #category : 'ui' }
FontFamily class >> readIfNeeded: dir [
	"Read from disk if not in memory"
	| fontNameFile fontName |
	fontNameFile := dir // 'fontName.txt'.
	fontName := fontNameFile exists
		ifTrue: [ fontNameFile textContents ]
		ifFalse: [ dir name ].
	(FontFamily familyNamed: fontName)
		ifNotNil: [ :it | ^it ].
	TrueTypeFontFamily readAllTrueTypeFontsIn: dir.
	^FontFamily familyNamed: fontName
]

{ #category : 'cached state access' }
FontFamily class >> releaseClassCachedState [

	AvailableFamilies keysAndValuesDo: [ :familyName :fontFamily |
		fontFamily releaseCachedState ]
]

{ #category : 'accessing' }
FontFamily >> atPointSize: aNumber [
"
	^baseFontBySizes at: aNumber ifAbsent: nil
"
	^baseFontBySizes at: aNumber ifAbsent: [ |found |
		found := nil.
		baseFontBySizes do: [ :font |
			(found isNil or: [ (found pointSize - aNumber) abs > (font pointSize - aNumber) abs ])
				ifTrue: [ found := font ]].
			found
		]
]

{ #category : 'accessing' }
FontFamily >> familyName [
	^ familyName
]

{ #category : 'initialization' }
FontFamily >> familyName: aString [
	familyName := aString.
	baseFontBySizes := Dictionary new
]

{ #category : 'accessing' }
FontFamily >> folderName [
	^nil
]

{ #category : 'testing' }
FontFamily >> isTrueTypeFontFamily [
	^false
]

{ #category : 'accessing' }
FontFamily >> pointSizes [
	^baseFontBySizes keys sort
]

{ #category : 'caching' }
FontFamily >> releaseCachedState [

	baseFontBySizes keysAndValuesDo: [ :size :font |
		font releaseCachedState ]
]

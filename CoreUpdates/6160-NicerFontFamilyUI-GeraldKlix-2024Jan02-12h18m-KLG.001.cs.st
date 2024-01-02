'From Cuis6.3 [latest update: #6159] on 2 January 2024 at 12:23:13 pm'!

!FontFamily class methodsFor: 'ui' stamp: 'jmv 1/2/2024 12:08:06'!
promptUser: currentNameOrNil includeNonASCII: showFontsNotIncludingASCIICharacters
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

	menu addTitle: 'Available to install '.
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
	menu lastSubmorph class == MenuLineMorph ifTrue: [
		3 timesRepeat: [menu lastSubmorph delete ]].

	answer := menu invokeModal.
	answer ifNil: [ ^nil ].
	answer first = #fontDirectory
		ifTrue: [ TrueTypeFontFamily readAllTrueTypeFontsIn: answer third ].
	^FontFamily familyNamed: answer second! !


!LabelMorph methodsFor: 'printing' stamp: 'jmv 1/2/2024 12:00:47'!
fontFamily: aFontFamily
	"Set the font my text will use. The emphasis remains unchanged."

	font := aFontFamily atPointSize: (font ifNotNil: [ font pointSize ] ifNil: [ FontFamily defaultPointSize ]).
	self font: font emphasis: emphasis! !


!FontFamily class methodsFor: 'ui' stamp: 'jmv 1/2/2024 12:09:05'!
promptUser
	"Present a menu of font families, answer selection.
	FontFamily promptUser
	"
	^self promptUser: FontFamily defaultFamilyName includeNonASCII: true! !

!FontFamily class methodsFor: 'ui' stamp: 'KLG 1/2/2024 12:08:48'!
promptUser: currentNameOrNil
	"
	FontFamily promptUser
	FontFamily promptUser: FontFamily defaultFamilyName
	"
	"Present a menu of available fonts, and if one is chosen, return it.
	Otherwise return nil."

	^self promptUser: currentNameOrNil includeNonASCII: true! !

!FontFamily class methodsFor: 'ui' stamp: 'KLG 1/2/2024 12:15:27'!
promptUserAlt
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
	^FontFamily familyNamed: fontMenu invokeModal.! !

!FontFamily class methodsFor: 'ui' stamp: 'jmv 1/2/2024 12:10:17'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	(self promptUser: FontFamily defaultFamilyName includeNonASCII: false) ifNotNil: [ :fontFamily |
		fontFamily includesAscii
			ifTrue: [PreferenceSet setDefaultFont: fontFamily familyName ]
			ifFalse: [self inform: 'Selected font does not include ASCII characters. Can not be set as default.' ]].! !


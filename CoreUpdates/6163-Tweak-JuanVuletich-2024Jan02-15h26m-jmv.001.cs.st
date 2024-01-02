'From Cuis6.3 [latest update: #6162] on 2 January 2024 at 3:30:44 pm'!

!FontFamily class methodsFor: 'ui' stamp: 'jmv 1/2/2024 15:29:52'!
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
	^FontFamily familyNamed: answer second! !


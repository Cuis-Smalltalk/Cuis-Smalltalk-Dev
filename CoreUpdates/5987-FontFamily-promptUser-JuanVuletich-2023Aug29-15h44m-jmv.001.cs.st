'From Cuis 6.0 [latest update: #5986] on 29 August 2023 at 4:49:36 pm'!

!FontFamily class methodsFor: 'ui' stamp: 'jmv 8/29/2023 16:28:29'!
addFontDir: dir to: menu current: currentNameOrNil
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
	^true! !

!FontFamily class methodsFor: 'ui' stamp: 'jmv 8/29/2023 16:40:01'!
promptUser: currentNameOrNil
	"
	FontFamily promptUser
	FontFamily promptUser: FontFamily defaultFamilyName
	"
	"Present a menu of available fonts, and if one is chosen, return it.
	Otherwise return nil."

	| menu answer |
	menu := MenuMorph new.
	menu defaultTarget: menu.
	menu addLabel: '-- Installed Fonts --'.
	menu addLine.
	FontFamily familyNames do: [ :fontName |
		menu
			add: (fontName=currentNameOrNil ifTrue: ['<on>'] ifFalse: ['<off>']), fontName
			target: menu
			action: #modalSelection:
			argument: {#fontName. fontName} ].

	menu addLine.
	menu addLabel: '-- Available to install --'.
	menu addLine.
	((DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts')
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


!MenuMorph methodsFor: 'construction' stamp: 'jmv 8/29/2023 16:26:04'!
add: aString action: aSymbol argument: arg
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: { arg }! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 8/28/2023 17:33:27'!
addLabel: aString

	^(self add: aString italic
		target: defaultTarget
		action: nil
		argumentList: #())
			setIcon: nil;
			isEnabled: false;
			color: Color black! !


!FontFamily class methodsFor: 'ui' stamp: 'jmv 8/29/2023 16:42:05'!
promptUser
	"Present a menu of font families, answer selection.
	FontFamily promptUser
	"
	^self promptUser: FontFamily defaultFamilyName! !

!FontFamily class methodsFor: 'ui' stamp: 'jmv 8/29/2023 16:43:54'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	self promptUser ifNotNil: [ :fontFamily |
		fontFamily includesAscii
			ifTrue: [PreferenceSet setDefaultFont: fontFamily familyName ]
			ifFalse: [self inform: 'Selected font does not include ASCII characters. Can not be set as default.' ]].! !

!FontFamily class methodsFor: 'ui' stamp: 'jmv 8/29/2023 16:47:06'!
readIfNeeded: dir
	"Read from disk if not in memory"
	| fontNameFile fontName |
	fontNameFile := dir // 'fontName.txt'.
	fontName := fontNameFile exists
		ifTrue: [ fontNameFile textContents ]
		ifFalse: [ dir name ].
	(FontFamily familyNamed: fontName)
		ifNotNil: [ :it | ^it ].
	TrueTypeFontFamily readAllTrueTypeFontsIn: dir.
	^FontFamily familyNamed: fontName! !

!methodRemoval: FontFamily class #promptUserFolders stamp: 'jmv 8/29/2023 16:44:09'!
FontFamily class removeSelector: #promptUserFolders!

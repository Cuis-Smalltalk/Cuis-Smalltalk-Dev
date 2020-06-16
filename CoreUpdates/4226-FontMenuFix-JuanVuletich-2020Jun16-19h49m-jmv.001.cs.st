'From Cuis 5.0 [latest update: #4225] on 16 June 2020 at 8:00:35 pm'!

!FontFamily class methodsFor: 'ui' stamp: 'jmv 6/16/2020 19:59:24'!
promptUser
	"Present a menu of font families, answer selection.
	FontFamily promptUser
	"
	| menu selectedNameOrDirectory familyName current |
	current _ (FontFamily familyNamed: FontFamily defaultFamilyName) folderName.
	menu _ MenuMorph new.
	((DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts')
		directories sorted: [ :a :b | a name < b name ]) do: [ :dir | | subDirs |
			subDirs _ dir directories sorted: [ :a :b | a name < b name ].
			subDirs isEmpty
				ifTrue: [ | this |
					this _ dir name.
					menu
						add: (this=current ifTrue: ['<on>'] ifFalse: ['<off>']), this
						target: menu
						action: #modalSelection:
						argument: dir ]
				ifFalse: [ | submenu this |
					submenu _ MenuMorph new.
					subDirs do: [ :subdir |
						this _ subdir name.
						submenu
							add: (this=current ifTrue: ['<on>'] ifFalse: ['<off>']), this
							target: menu
							action: #modalSelection:
							argument: subdir ].
					menu add: dir name, '... ' subMenu: submenu
					]].
	FontFamily availableFamilies values do: [ :family |
		family folderName isNil ifTrue: [
			familyName _ family familyName.
			menu
				add: (familyName = FontFamily defaultFamilyName ifTrue: ['<on>'] ifFalse: ['<off>']), familyName
				target: menu
				action: #modalSelection:
				argument: familyName ]].
	selectedNameOrDirectory _ menu invokeModal.
	selectedNameOrDirectory isNil ifTrue: [ ^nil ].
	(FontFamily familyNamed: selectedNameOrDirectory)
		ifNotNil: [ :it | ^it ].
	^FontFamily availableFamilies values
		detect: [ :any | any folderName = selectedNameOrDirectory name ]
		ifNone: [
			Feature require: 'VectorGraphics'.
			FontFamily familyNamed: ((Smalltalk at: #TrueTypeFontFamily) read: selectedNameOrDirectory) anyOne ]! !


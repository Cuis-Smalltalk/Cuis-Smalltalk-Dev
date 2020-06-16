'From Cuis 5.0 [latest update: #4222] on 16 June 2020 at 6:59:04 pm'!

!FontFamily methodsFor: 'accessing' stamp: 'jmv 6/16/2020 18:30:05'!
folderName
	^nil! !


!FontFamily class methodsFor: 'accessing' stamp: 'jmv 6/16/2020 18:43:47'!
availableFamilies
	^AvailableFamilies! !

!FontFamily class methodsFor: 'ui' stamp: 'jmv 6/16/2020 18:56:36'!
promptUserAlt
	"Present a menu of font families, answer selection.
	Alternative implementation: Show only installed fonts.
	FontFamily promptUserAlt
	"
	| fontMenu priorFontFamily |
	priorFontFamily _ FontFamily defaultFamilyName.
	fontMenu _ MenuMorph new.
	FontFamily familyNames do: [:fontName |
		fontMenu
			add: ((priorFontFamily sameAs: fontName) ifTrue: ['<on>'] ifFalse: ['<off>'] ), fontName
			target: fontMenu
			action: #modalSelection:
			argument: fontName.
	].
	^FontFamily familyNamed: fontMenu invokeModal.! !


!FontFamily class methodsFor: 'ui' stamp: 'jmv 6/16/2020 18:58:14'!
promptUser
	"Present a menu of font families, answer selection.
	FontFamily promptUser
	"
	| menu selectedDirectory dejaVuBitmap current this |
	current _ (FontFamily familyNamed: FontFamily defaultFamilyName) folderName.
	menu _ MenuMorph new.
	((DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts')
		directories sorted: [ :a :b | a name < b name ]) do: [ :dir | | subDirs |
			subDirs _ dir directories sorted: [ :a :b | a name < b name ].
			subDirs isEmpty
				ifTrue: [
					this _ dir name.
					menu
						add: (this=current ifTrue: ['<on>'] ifFalse: ['<off>']), this
						target: menu
						action: #modalSelection:
						argument: dir ]
				ifFalse: [ | submenu |
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
	dejaVuBitmap _ 'DejaVu Bitmap'.
	menu
		add: (dejaVuBitmap = FontFamily defaultFamilyName ifTrue: ['<on>'] ifFalse: ['<off>']), dejaVuBitmap
		target: menu
		action: #modalSelection:
		argument: this.
	selectedDirectory _ menu invokeModal.
	selectedDirectory = this
		ifTrue: [ ^FontFamily familyNamed: dejaVuBitmap ].
	selectedDirectory isNil ifTrue: [ ^nil ].
	^FontFamily availableFamilies values
		detect: [ :any | any folderName = selectedDirectory name ]
		ifNone: [
			Feature require: 'VectorGraphics'.
			FontFamily familyNamed: ((Smalltalk at: #TrueTypeFontFamily) read: selectedDirectory) anyOne ]! !

!FontFamily class methodsFor: 'ui' stamp: 'jmv 6/13/2020 17:32:58'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	self promptUser ifNotNil: [ :fontFamily | Preferences setDefaultFont: fontFamily familyName ]! !

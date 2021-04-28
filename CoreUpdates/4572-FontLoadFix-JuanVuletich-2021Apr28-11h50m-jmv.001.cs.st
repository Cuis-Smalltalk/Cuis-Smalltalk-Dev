'From Cuis 5.0 [latest update: #4571] on 28 April 2021 at 11:51:28 am'!

!FontFamily class methodsFor: 'ui' stamp: 'jmv 4/28/2021 11:38:29'!
promptUserFolders
	"Present a menu of font folders, answer selection.
	FontFamily promptUserFolders
	"
	| menu familyName current |
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
	^menu invokeModal! !


!FontFamily class methodsFor: 'file read write' stamp: 'jmv 4/28/2021 11:25:43'!
readAdditionalTrueTypeFonts
	Feature require: 'VectorGraphics'.
	UISupervisor whenUIinSafeState: [
		Smalltalk at: #TrueTypeFontFamily ifPresent: [ :cls | cls readAdditionalFonts ]]! !

!FontFamily class methodsFor: 'ui' stamp: 'jmv 4/28/2021 11:41:10'!
promptUser
	"Present a menu of font families, answer selection.
	FontFamily promptUser
	"
	| selectedNameOrDirectory |
	selectedNameOrDirectory _ self promptUserFolders.
	selectedNameOrDirectory isNil ifTrue: [ ^nil ].
	(FontFamily familyNamed: selectedNameOrDirectory)
		ifNotNil: [ :it | ^it ].
	^FontFamily availableFamilies values
		detect: [ :any | any folderName = selectedNameOrDirectory name ]
		ifNone: [
			FontFamily familyNamed: ((Smalltalk at: #TrueTypeFontFamily) read: selectedNameOrDirectory) anyOne ]! !

!FontFamily class methodsFor: 'ui' stamp: 'jmv 4/28/2021 11:51:19'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	| selectedNameOrDirectory fontFamily |
	selectedNameOrDirectory _ self promptUserFolders.
	selectedNameOrDirectory isNil ifTrue: [ ^nil ].
	(FontFamily familyNamed: selectedNameOrDirectory)
		ifNotNil: [ :it | ^Preferences setDefaultFont: it familyName ].
	Feature require: 'VectorGraphics'.
	UISupervisor whenUIinSafeState: [
		fontFamily _ FontFamily availableFamilies values
			detect: [ :any | any folderName = selectedNameOrDirectory name ]
			ifNone: [
				FontFamily familyNamed: ((Smalltalk at: #TrueTypeFontFamily) read: selectedNameOrDirectory) anyOne ].
		Preferences setDefaultFont: fontFamily familyName ]! !


'From Cuis7.5 [latest update: #7746] on 9 December 2025 at 1:05:17 pm'!

!Theme class methodsFor: 'user interface' stamp: 'BB 4/Dec/2025 19:53:11'!
changeTheme

	| menu |
	menu := MenuMorph new.
	menu
		addTitle: 'Choose a theme';
		addStayUpIcons.
	(Theme allSubclasses sorted: [ :a :b | a name < b name ]) do: [ :themeClass |
		menu add: themeClass name target: themeClass action: #beCurrent ].
	
	(FeatureRequirement name: 'Theme-Themes') isAlreadySatisfied
		ifTrue: [
			menu addLine.
			PersonalizedTheme savedThemes keysDo: [ :themeName |
				menu 
					add: themeName
					target: PersonalizedTheme
					action: #loadThemeNamed:
					argument: themeName
			].
		]
		ifFalse: [
			menu addLine.
			menu add: '* Load Additional Themes *' target: self action: #loadAdditionalThemes ].

	menu popUpInWorld: self runningWorld.! !


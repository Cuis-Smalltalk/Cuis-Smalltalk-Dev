'From Cuis7.5 [latest update: #7758] on 15 December 2025 at 10:27:20 am'!

!Theme class methodsFor: 'user interface' stamp: 'jmv 15/Dec/2025 10:27:02'!
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
			Smalltalk at: #PersonalizedTheme ifPresent: [ :ptClass |
				ptClass savedThemes keysDo: [ :themeName |
					menu 
						add: themeName
						target: ptClass
						action: #loadThemeNamed:
						argument: themeName ]]]
		ifFalse: [
			menu addLine.
			menu add: '* Load Additional Themes *' target: self action: #loadAdditionalThemes ].

	menu popUpInWorld: self runningWorld.! !


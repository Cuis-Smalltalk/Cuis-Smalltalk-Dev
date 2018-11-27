'From Cuis 5.0 of 7 November 2016 [latest update: #3505] on 27 November 2018 at 2:46:58 pm'!

!Theme class methodsFor: 'instance creation' stamp: 'jmv 11/27/2018 14:45:06'!
beCurrent
	self currentTheme: self.
	self inform: 'Please close and reopen all windows'! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 11/27/2018 14:42:55'!
changeTheme

	| menu |
	"In Theme-Themes.pck.st"
	(FeatureRequirement name: 'Theme-Themes') isAlreadySatisfied ifFalse: [
		(PopUpMenu 
			confirm: 'The Additional Themes package is not loaded', String newLineString, 'Would you like me to load it for you now?')
				ifTrue: [Feature require: #'Theme-Themes'] 
	].
	menu _ MenuMorph new.
	menu
		addTitle: 'Choose a theme';
		addStayUpIcons.
	(Theme allSubclasses sorted: [ :a :b | a name < b name ]) do: [ :themeClass |
		menu add: themeClass name target: themeClass action: #beCurrent ].
	menu popUpInWorld: self runningWorld! !


'From Cuis 6.0 [latest update: #5608] on 29 December 2022 at 1:04:02 pm'!

!Theme class methodsFor: 'user interface' stamp: 'jmv 12/29/2022 13:03:07'!
loadAdditionalThemes
	"Load additional themes and reopen menu."
	Feature require: #'Theme-Themes'.
	self changeTheme! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 12/29/2022 13:03:40'!
changeTheme

	| menu |
	menu _ MenuMorph new.
	menu
		addTitle: 'Choose a theme';
		addStayUpIcons.
	(Theme allSubclasses sorted: [ :a :b | a name < b name ]) do: [ :themeClass |
		menu add: themeClass name target: themeClass action: #beCurrent ].
	(FeatureRequirement name: 'Theme-Themes') isAlreadySatisfied ifFalse: [
		menu addLine.
		menu add: '* Load Additional Themes *' target: self action: #loadAdditionalThemes ].
	menu popUpInWorld: self runningWorld! !


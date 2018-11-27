'From Cuis 5.0 of 7 November 2016 [latest update: #3500] on 27 November 2018 at 10:16:21 am'!
!classDefinition: #BrightTheme category: #'Theme-Core'!
Theme subclass: #BrightTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Theme-Core'!

!BrightTheme commentStamp: '<historical>' prior: 0!
Default bright colored theme for Cuis.!


!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 11/26/2018 18:00:55'!
generateShoutConfig

	| styles colors |
	styles := OrderedCollection new.
	colors := self shout as: Dictionary.

	{
		{self undefined. colors at: #undefined}.
		{self defaults . colors at: #defaults}.
		{self pseudoVariables . colors at: #pseudoVariables}.
		{self literals . colors at: #literals}.
		{self instVar . colors at: #instVar}.
		{self messages . colors at: #messages}.
		{self blockLevelZero . colors at: #blockLevelZero}.
		{self blockLevelOne . colors at: #blockLevelOne}.
		{self blockLevelTwo . colors at: #blockLevelTwo}.
		{self blockLevelThree . colors at: #blockLevelThree}.
		{self blockLevelFour . colors at: #blockLevelFour}.
		{self blockLevelFive . colors at: #blockLevelFive}.
		{self blockLevelSix . colors at: #blockLevelSix}.
		{self blockLevelSeven . colors at: #blockLevelSeven}.
		{self tempBar . colors at: #tempBar}.
		{self methodTags . colors at: #methodTags . #bold}.
		{self globals . colors at: #defaults . #bold}.
		{self incompleteMessages . colors at: #incompleteMessages . #underlined}.
		{self argumentTypes . colors at: #arguments . self italic}.
		{self symbols . colors at: #messages . #bold}.
		{self pattern . colors at: #selector . #bold}.
		{self ansiAssignment . nil . #bold}.
		{self assignment . nil . #(#bold #withST80Glyphs)}.
		{self return . nil . #(#bold #withST80Glyphs)}.
		{self tempVars . colors at: #tempVars . self italic}.
		{self blockTemps . colors at: #tempBar . self italic}
	} do: [ :style |
		styles addAll:
			(style first
				collect: [ :category | | elements |
					elements _ style asOrderedCollection.
					elements at: 1 put: category.
					Array withAll: elements ])].

	"Miscellaneous remainder after factoring out commonality:"
	styles addAll: {
		{#unfinishedString . colors at: #undefined . #normal}.
		{#undefinedIdentifier . colors at: #undefined .#bold}.
		{#unfinishedComment . colors at: #pseudoVariables . self italic}.
		{#comment . colors at: #comment . self italic}.
		{#string . colors at: #instVar . #normal}.
		{#literal . nil . self italic}.
		{#incompleteIdentifier . colors at: #tempVars . {#italic. #underlined}}.
		{#classVar . colors at: #tempVars . #bold}.
	}.

	^ styles! !

!Theme methodsFor: 'shout' stamp: 'jmv 11/27/2018 09:59:21'!
shout
	"Color symbols as an association list.
	SHTextStylerST80 initialize
	"
	^ {
		#selector  					-> nil.
		#arguments 					-> #(cyan muchDarker).
		#comment 					-> #(green muchDarker).
		#tempBar 					-> #gray.
		#tempVars 					-> #(gray muchDarker).
		#instVar 					-> #(magenta muchDarker).
		#pseudoVariables 			-> #(red muchDarker).
		#literals 					-> #(green muchDarker).
		#messages 					-> #(blue darker).
		#incompleteMessages 		-> #(gray veryMuchDarker).
		#blockLevelZero 			-> #black.
		#blockLevelOne 			-> #brown.
		#blockLevelTwo 			-> #magenta.
		#blockLevelThree 			-> #red.
		#blockLevelFour 			-> #(orange darker).
		#blockLevelFive 			-> #(orange muchDarker).
		#blockLevelSix 			-> #(green muchDarker).
		#blockLevelSeven 			-> #blue.
		#defaults 					-> #black.
		#undefined 					-> #red.
		#methodTags 				-> #(green muchDarker).
	}! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 11/27/2018 10:14:54'!
changeTheme

	| menu |
	menu _ MenuMorph new.
	menu
		addTitle: 'Choose a theme';
		addStayUpIcons.
	(Theme allSubclasses sorted: [ :a :b | a name < b name ]) do: [ :themeClass |
		menu add: themeClass name target: themeClass action: #beCurrent ].
	menu popUpInWorld: self runningWorld! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
BrightTheme beCurrent.!


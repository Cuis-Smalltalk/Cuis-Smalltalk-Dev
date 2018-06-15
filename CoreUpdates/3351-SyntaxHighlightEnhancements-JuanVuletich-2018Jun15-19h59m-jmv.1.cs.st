'From Cuis 5.0 of 7 November 2016 [latest update: #3350] on 15 June 2018 at 8:06:45 pm'!

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 6/15/2018 19:47:32'!
blockLevelOne

	^ #(
		blockStart1
		blockEnd1
		leftParenthesis1
		rightParenthesis1
	)! !

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 6/15/2018 19:44:15'!
blockLevelThree

	^ #(
		blockStart3
		blockEnd3
		leftParenthesis3
		rightParenthesis3
	)
! !

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 6/15/2018 19:45:51'!
blockLevelTwo

	^ #(
		blockStart2
		blockEnd2
		leftParenthesis2
		rightParenthesis2
	)
! !

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 6/15/2018 19:53:16'!
blockLevelZero

	^ #(
		blockStart
		blockEnd
		leftParenthesis
		rightParenthesis
	)! !


!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 6/15/2018 19:53:22'!
defaults

	^ #(
		default
		arrayStart
		arrayEnd
		arrayStart1
		arrayEnd1
		leftBrace
		rightBrace
		cascadeSeparator
		chainSeparator
		statementSeparator
		externalCallType
		externalCallTypePointerIndicator
		blockArgColon
	)! !

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 6/15/2018 20:06:29'!
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
		{self pattern . nil . #bold}.
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

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 6/15/2018 19:45:42'!
instVar
	^ #(
		instVar
	)! !

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 6/15/2018 19:44:23'!
literals

	^ #(
			character
			integer
			number
			-
	)! !

!Theme methodsFor: 'shout' stamp: 'jmv 6/15/2018 20:06:11'!
shout
	"Color symbols as an association list.
	SHTextStylerST80 initialize
	"
	
	^ {
		#defaults 					-> #black.
		#undefined 				-> #red.
		#comment 					-> #(green muchDarker).
		#methodTags 			-> #(green muchDarker).
		#pseudoVariables 		-> #(red muchDarker).
		#literals 					-> #(green muchDarker).
		#messages 				-> #(blue darker).
		#arguments 				-> #(cyan muchDarker).
		#instVar 					-> #(magenta muchDarker).
		#incompleteMessages 	-> #(gray veryMuchDarker).
		#blockLevelZero 			-> #black.
		#blockLevelOne 			-> #brown.
		#blockLevelTwo 			-> #magenta.
		#blockLevelThree 		-> #red.
		#blockLevelFour 			-> #(orange darker).
		#blockLevelFive 			-> #(orange muchDarker).
		#blockLevelSix 			-> #(green muchDarker).
		#blockLevelSeven 		-> #blue.
		#tempBar 					-> #gray.
		#tempVars 				-> #(gray muchDarker).
	}! !

!methodRemoval: Theme #firstBlockLevel!
Theme removeSelector: #firstBlockLevel!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	SHTextStylerST80 initialize!


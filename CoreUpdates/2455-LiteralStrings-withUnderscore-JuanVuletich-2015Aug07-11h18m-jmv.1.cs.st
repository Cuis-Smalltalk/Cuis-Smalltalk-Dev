'From Cuis 4.2 of 25 July 2013 [latest update: #2453] on 7 August 2015 at 11:20:27.75659 am'!

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 8/5/2015 18:33'!
generateShoutConfig

	| styles colors |
	
	styles := OrderedCollection new.
	colors := self shout as: Dictionary.

	{
		{self undefined. colors at: #undefined. #withUnderscoreGlyphs}.
		{self literals . colors at: #pseudoVariables}.
		{self defaults . colors at: #defaults}.
		{self pseudoVariables . colors at: #pseudoVariables}.
		{self blockLevelFour . colors at: #blockLevelFour}.
		{self instVar . colors at: #instVar}.
		{self messages . colors at: #messages. #withUnderscoreGlyphs}.
		{self blockLevelFive . colors at: #blockLevelFive}.
		{self blockLevelSix . colors at: #blockLevelSix}.
		{self blockLevelSeven . colors at: #blockLevelSeven}.
		{self tempBar . colors at: #tempBar}.
		{self methodTags . colors at: #methodTags . {#bold. #withUnderscoreGlyphs}}.
		{self globals . colors at: #defaults . {#bold. #withUnderscoreGlyphs}}.
		{self incompleteMessages . colors at: #incompleteMessages . {#underlined. #withUnderscoreGlyphs}}.
		{self argumentTypes . colors at: #arguments . {self italic. #withUnderscoreGlyphs}}.
		{self symbols . colors at: #messages . {#bold. #withUnderscoreGlyphs}}.
		{self pattern . nil . {#bold. #withUnderscoreGlyphs}}.
		{self ansiAssignment . nil . #bold}.
		{self assignment . nil . #bold}.
		{self return . nil . #bold}.
		{self tempVars . colors at: #tempVars . {self italic. #withUnderscoreGlyphs}}.
		{self blockTemps . colors at: #tempBar . {self italic. #withUnderscoreGlyphs}}
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
		{#undefinedIdentifier . colors at: #undefined . {#bold. #withUnderscoreGlyphs}}.
		{#unfinishedComment . colors at: #pseudoVariables . {self italic. #withUnderscoreGlyphs}}.
		{#comment . colors at: #methodTags . {self italic. #withUnderscoreGlyphs}}.
		{#string . colors at: #instVar . { #normal. #withUnderscoreGlyphs}}.
		{#literal . nil . self italic}.
		{#incompleteIdentifier . colors at: #tempVars . {#italic. #underlined. #withUnderscoreGlyphs}}.
		{#classVar . colors at: #tempVars . #bold}.
	}.

	^ styles! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
		SHTextStylerST80 initialize!


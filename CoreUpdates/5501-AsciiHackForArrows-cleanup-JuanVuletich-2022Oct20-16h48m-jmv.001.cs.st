'From Cuis 6.0 [latest update: #5498] on 20 October 2022 at 4:51:29 pm'!

!TextEmphasis commentStamp: 'jmv 10/20/2022 16:40:47' prior: 0!
A TextEmphasis, encodes a characteristic applicable to all fonts.  The encoding is as follows:
	1	bold
	2	italic
	4	underlined		
	8	struck out
	16 	Superscript
	32 	Subscript!


!TextEditor methodsFor: 'attributes' stamp: 'jmv 10/20/2022 16:41:46'!
offerFontStyleMenu
	"This is a user command, and generates undo"

	| emphases menuStrings entries reply code startIndex attribute |
	startIndex _ self startIndex.
	code _ model actualContents emphasisAt: startIndex.
	emphases _ #(bold italic underlined struckThrough superscript subscript).
	menuStrings _ Array streamContents: [ :strm |
		strm nextPut:(code isZero ifTrue: ['<on>'] ifFalse: ['<off>']), 'normal'.
		emphases do: [ :emph |
			strm nextPut:
				((code anyMask: (TextEmphasis perform: emph) emphasisCode)
					ifTrue: ['<on>'] ifFalse: ['<off>']),
				emph asPlainString ]].
	entries _ `#(normal)`, emphases.
	reply _ (SelectionMenu labelList: menuStrings lines: #(1) selections: entries) startUpMenu.
	reply ifNotNil: [
		attribute _ TextEmphasis perform: reply.
		((menuStrings at: (entries indexOf: reply)) beginsWith: '<on>')
			ifTrue: [ self unapplyAttributes: {attribute} ]
			ifFalse: [ self applyAttribute:  attribute ]].
	morph updateFromTextComposition.! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 10/20/2022 16:41:36'!
changeEmphasis: aKeyboardEvent
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"control 0..9 -> 0..9"
	"This is a user command, and generates undo"

	| keyCode attributeToAdd attributeToRemove oldAttributes |
	keyCode _ ('0123456'
		indexOf: aKeyboardEvent keyCharacter
		ifAbsent: [ 1 ]) - 1.
	oldAttributes _ emphasisHere.

	keyCode = 0 ifTrue: [ attributeToAdd _ TextEmphasis normal ].

	(keyCode between: 1 and: 7) ifTrue: [
		attributeToAdd _ TextEmphasis perform: (#(#bold #italic #underlined #struckThrough #superscript #subscript) at: keyCode).
		oldAttributes do: [ :oldAtt |
			oldAtt = attributeToAdd ifTrue: [
				attributeToAdd _ nil.
				attributeToRemove _ oldAtt ]]].

	attributeToAdd ifNotNil: [ self applyAttribute: attributeToAdd ].
	attributeToRemove ifNotNil: [ self unapplyAttributes: {attributeToRemove} ].
	^ true! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'jmv 10/20/2022 16:45:18'!
prefProgramming
	^ #(
		#(atMinusDigitMeaning st80 #(st80 ansiSmalltalk disabled) )
		#(allowBlockArgumentAssignment false )
		#(alternativeBrowseIt false )
		#(backgroundColorFillsAllBackground true )
		#(browseWithPrettyPrint false )
		#(classAnnotations #(instanceMethodsCount classMethodsCount linesOfCode) )
		#(debugHaloHandle true )
		#(debugLogTimestamp true )
		#(debugShowDamage false )
		#(decorateBrowserButtons true )
		#(diffsInChangeList true )
		#(diffsWithPrettyPrint false )
		#(extraDebuggerButtons true )
		#(fullPrintItInWorkspaces false )
		#(highlightBlockNesting true )
		#(italicsInShout true)
		#(leftArrowAssignmentsInGeneratedCode false )
		#(listClassesHierarchically true )
		#(logDebuggerStackToFile false )
		#(methodAnnotations #(timeStamp linesOfCode messageCategory implementorsCount sendersCount  packages changeSets) )	
		#(prettyPrintRectangularBlocks false )
		#(shiftClickShowsImplementors false )
		#(shoutInWorkspaces true )
		#(showAnnotations true )
		#(showLinesInHierarchyViews true )
		#(stylingWithEmphasisInWorkspaces false )
		#(systemCategoryAnnotations #(classCount instanceMethodsCount classMethodsCount linesOfCode) )
		#(syntaxHighlightingAsYouType true )
		#(syntaxHighlightingAsYouTypeAnsiAssignment false )
		#(syntaxHighlightingAsYouTypeLeftArrowAssignment false )
		#(thoroughSenders true )
		#(usePreDebugWindow false)
	)! !


!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 10/20/2022 16:48:46'!
emphasized: code
	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis derivedFrom |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].

	derivativeFonts ifNil: [ derivativeFonts _ Dictionary new ].
	derivative _ derivativeFonts at: code ifPresent: [ :der | ^ der ].		"Already have this style"

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis _ 1 bitShift: code highBit - 1.
	derivedFrom _ self emphasized: code - addedEmphasis.

	"Order is Bold(B), Italic(i), Underlined(U), StruckThrough(X), Superscript(Sup), Subscript(Sub)"
	derivative _ addedEmphasis caseOf: {
		[ 1 ] -> [ derivedFrom syntheticBold ].
		[ 2 ] -> [ derivedFrom syntheticItalic ].
		[ 4 ] -> [ derivedFrom syntheticUnderlined ].
		[ 8 ] -> [ derivedFrom syntheticStruckThrough ].
		[ 16 ] -> [ derivedFrom syntheticSuperscript ].
		[ 32 ] -> [ derivedFrom syntheticSubscript ].
		[ 64 ] -> [ derivedFrom "syntheticST80Glyphs" ] }.

	derivative baseFont: self.
	derivative emphasis: code.
	derivativeFonts at: code put: derivative.
	^ derivative! !


!StrikeFont class methodsFor: 'character shapes' stamp: 'jmv 10/20/2022 16:49:34'!
useUnderscore
	"Sets underscore and caret glyphs for chars 95 and 94. 
	ASCII standard glyphs"
	"
	StrikeFont allInstancesDo: [ :font | font reset ]. StrikeFont useUnderscore
	"
	self allInstances do: [ :font | font useUnderscore ]! !


!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 10/20/2022 16:42:38'!
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
		{self assignment . nil . #bold}.
		{self return . nil . #bold}.
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

!methodRemoval: StrikeFont class #assignmentGlyphSelectorPreferenceChanged stamp: 'jmv 10/20/2022 16:51:15'!
StrikeFont class removeSelector: #assignmentGlyphSelectorPreferenceChanged!
!methodRemoval: StrikeFont #useAlwaysLeftArrow stamp: 'jmv 10/20/2022 16:50:12'!
StrikeFont removeSelector: #useAlwaysLeftArrow!
!methodRemoval: StrikeFont #useLeftArrow stamp: 'jmv 10/20/2022 16:50:18'!
StrikeFont removeSelector: #useLeftArrow!
!methodRemoval: StrikeFont #syntheticST80Glyphs stamp: 'jmv 10/20/2022 16:50:56'!
StrikeFont removeSelector: #syntheticST80Glyphs!
!methodRemoval: AbstractFont class #withST80GlyphsCode stamp: 'jmv 10/20/2022 16:50:29'!
AbstractFont class removeSelector: #withST80GlyphsCode!
!methodRemoval: AbstractFont class #assignmentGlyphSelectorPreferenceChanged stamp: 'jmv 10/20/2022 16:51:05'!
AbstractFont class removeSelector: #assignmentGlyphSelectorPreferenceChanged!
!methodRemoval: PreferenceSet class #assignmentGlyph: stamp: 'jmv 10/20/2022 16:50:46'!
PreferenceSet class removeSelector: #assignmentGlyph:!
!methodRemoval: TextEmphasis class #withST80Glyphs stamp: 'jmv 10/20/2022 16:50:26'!
TextEmphasis class removeSelector: #withST80Glyphs!

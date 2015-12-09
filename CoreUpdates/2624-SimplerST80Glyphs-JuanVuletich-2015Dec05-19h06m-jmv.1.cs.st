'From Cuis 4.2 of 25 July 2013 [latest update: #2623] on 5 December 2015 at 7:21:16.154335 pm'!

!AbstractFont class methodsFor: 'constants' stamp: 'jmv 12/5/2015 18:34'!
subscriptCode
	^32! !

!AbstractFont class methodsFor: 'constants' stamp: 'jmv 12/5/2015 18:33'!
superscriptCode
	^16! !

!AbstractFont class methodsFor: 'constants' stamp: 'jmv 12/5/2015 18:34'!
withST80GlyphsCode
	^64! !


!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/5/2015 18:44'!
boldDerivative
	"Build and answer a derivative that is Bold."
	| derivative |
	derivative _ self copy.
	derivative
		ensureCleanBold;
		name: self name , 'B';
		makeBoldGlyphs.! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/5/2015 18:53'!
syntheticBold
	"Build and answer a derivative that is Bold."

	| derivative |
	derivative _ self copy.
	derivative
		ensureCleanBold;
		name: self name , 'B';
		makeBoldGlyphs.
	^ derivative! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/5/2015 18:53'!
syntheticItalic
	"Build and answer a derivative that is Italic."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'I';
		makeItalicGlyphs.
	^ derivative! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/5/2015 19:12'!
syntheticST80Glyphs
	"Build and answer a derivative that includes ST-80 glyphs:
	a left arrow instead of the underscore, and an up arrow instead of the caret."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'ST80';
		useLeftArrow.
	^ derivative! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/5/2015 18:53'!
syntheticStruckThrough
	"Build and answer a derivative that is StruckThrough."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'X';
		makeStruckThroughGlyphs.
	^ derivative! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/5/2015 18:54'!
syntheticSubscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'Sub'.
	^ derivative! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/5/2015 18:54'!
syntheticSuperscript
	"Build and answer a derivative that is Superscript."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'Sup'.
	^ derivative! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/5/2015 18:53'!
syntheticUnderlined
	"Build and answer a derivative that is Underlined."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'U';
		makeUnderlinedGlyphs.
	^ derivative! !

!StrikeFont methodsFor: 'copying' stamp: 'jmv 12/5/2015 19:14'!
postCopy
	characterToGlyphMap _ nil! !


!TextEmphasis class methodsFor: 'instance creation' stamp: 'jmv 12/5/2015 18:34'!
subscript
	^ self new emphasisCode: AbstractFont subscriptCode! !

!TextEmphasis class methodsFor: 'instance creation' stamp: 'jmv 12/5/2015 18:34'!
superscript
	^ self new emphasisCode: AbstractFont superscriptCode! !

!TextEmphasis class methodsFor: 'instance creation' stamp: 'jmv 12/5/2015 18:34'!
withST80Glyphs
	^ self new emphasisCode: AbstractFont withST80GlyphsCode! !


!AbstractFont methodsFor: 'displaying' stamp: 'jmv 12/5/2015 19:10'!
on: aGrafPort displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| char fallbackFont x y answer |
	fallbackFont _ (AbstractFont familyName: 'DejaVu' aroundPointSize: self pointSize*0.9) emphasized: self emphasis.
	x _ p x.
	y _ p y.
	firstIndex to: lastIndex do: [ :i |
		char _ aString at: i.
		answer _ aGrafPort displayString: char asString from: 1 to: 1 at: x@y strikeFont: fallbackFont color: color.
		x _ x + (self widthOf: char) ].
	^answer! !


!StrikeFont methodsFor: 'accessing' stamp: 'jmv 12/5/2015 18:36'!
derivativeFonts
	^derivativeFonts! !

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/5/2015 18:38'!
derivativeFont: aStrikeFont at: index

	| newDeriv |
	(aStrikeFont isNil and: [ index = 0 ]) 
		ifTrue: [
			derivativeFonts _ nil.
			^ self].

	self isBaseFont ifFalse: [ 
		derivativeFonts _ nil.
		self error: 'Derivative fonts can not have derivatives' ].
	
	derivativeFonts ifNil: [derivativeFonts := Array new: 128].
	derivativeFonts size < 128 ifTrue: [
		newDeriv := Array new: 128.
		newDeriv replaceFrom: 1 to: derivativeFonts size with: derivativeFonts.
		derivativeFonts := newDeriv.
	].
	derivativeFonts at: index put: aStrikeFont.
	aStrikeFont ifNotNil: [ aStrikeFont baseFont: self ]! !

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/5/2015 18:58'!
emphasized: code
	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis derivedFrom safeCode |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].

	derivativeFonts ifNil: [derivativeFonts := Array new: 128].
	derivativeFonts size < 128 ifTrue: [
		derivativeFonts := derivativeFonts, (Array new: 128-derivativeFonts size)].

	derivative _ derivativeFonts at: (safeCode _ code min: derivativeFonts size).
	derivative ifNotNil: [^ derivative].  "Already have this style"

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis _ 1 bitShift: safeCode highBit - 1.
	derivedFrom _ self emphasized: safeCode - addedEmphasis.

	"Order is Bold(B), Italic(i), Underlined(U), StruckThrough(X), Superscript(Sup), Subscript(Sub), WithST80Glyphs(ST80)"
	derivative _ addedEmphasis caseOf: {
		[ 1 ] -> [ derivedFrom syntheticBold ].
		[ 2 ] -> [ derivedFrom syntheticItalic ].
		[ 4 ] -> [ derivedFrom syntheticUnderlined ].
		[ 8 ] -> [ derivedFrom syntheticStruckThrough ].
		[ 16 ] -> [ derivedFrom syntheticSuperscript ].
		[ 32 ] -> [ derivedFrom syntheticSubscript ].
		[ 64 ] -> [ derivedFrom syntheticST80Glyphs ] }.

	derivative baseFont: self.
	derivative emphasis: safeCode.
	derivativeFonts at: safeCode put: derivative.
	^ derivative! !

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/5/2015 18:37'!
reset
	"Reset the cache of derivative emphasized fonts"
	
	derivativeFonts ifNotNil: [
		derivativeFonts withIndexDo: [ :f :i |
			(f notNil and: [ f isSynthetic ]) 
				ifTrue: [ derivativeFonts at: i put: nil ]].
		"all after 16 are synthetic, and can be recreated on demand"
		derivativeFonts size > 16 ifTrue: [
			derivativeFonts _ derivativeFonts copyFrom: 1 to: 16 ]]! !

!StrikeFont methodsFor: 'displaying' stamp: 'jmv 12/5/2015 19:10'!
on: aGrafPort displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position.
	Answer nil if nothing was done."

	^aGrafPort
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p
		strikeFont: self
		color: color! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 12/5/2015 19:00'!
changeEmphasis: aKeyboardEvent
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"control 0..9 -> 0..9"
	"This is a user command, and generates undo"

	| keyCode attributeToAdd attributeToRemove oldAttributes |
	keyCode _ ('01234567'
		indexOf: aKeyboardEvent keyCharacter
		ifAbsent: [ 1 ]) - 1.
	oldAttributes _ emphasisHere.

	keyCode = 0 ifTrue: [ attributeToAdd _ TextEmphasis normal ].

	(keyCode between: 1 and: 7) ifTrue: [
		attributeToAdd _ TextEmphasis perform: (#(#bold #italic #underlined #struckThrough #superscript #subscript #withST80Glyphs) at: keyCode).
		oldAttributes do: [ :oldAtt |
			oldAtt = attributeToAdd ifTrue: [
				attributeToAdd _ nil.
				attributeToRemove _ oldAtt ]]].

	attributeToAdd ifNotNil: [ self applyAttribute: attributeToAdd ].
	attributeToRemove ifNotNil: [ self unapplyAttribute: attributeToRemove ].
	^ true! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 12/5/2015 18:31'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$0 	changeEmphasis:			'Normal')
		#(		$1 	changeEmphasis:			'Bold')
		#(		$2 	changeEmphasis:			'Italic')
		#(		$3 	changeEmphasis:			'Underline')
		#(		$4 	changeEmphasis:			'Strikeout')
		#(		$5 	changeEmphasis:			'Superscript')
		#(		$6 	changeEmphasis:			'Subscript')
		#(		$7 	changeEmphasis:			'With ST-80 Glyphs')
		#(		$8	#offerColorMenu:		'Change color')

		#(		$k	#offerFontMenu:			'Set font')

		#(		$u	#align:						'Toggle alignment')
	)! !


!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 12/5/2015 19:17'!
generateShoutConfig

	| styles colors |
	
	styles := OrderedCollection new.
	colors := self shout as: Dictionary.

	{
		{self undefined. colors at: #undefined}.
		{self literals . colors at: #pseudoVariables}.
		{self defaults . colors at: #defaults}.
		{self pseudoVariables . colors at: #pseudoVariables}.
		{self blockLevelFour . colors at: #blockLevelFour}.
		{self instVar . colors at: #instVar}.
		{self messages . colors at: #messages}.
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
		{#comment . colors at: #methodTags . self italic}.
		{#string . colors at: #instVar . #normal}.
		{#literal . nil . self italic}.
		{#incompleteIdentifier . colors at: #tempVars . {#italic. #underlined}}.
		{#classVar . colors at: #tempVars . #bold}.
	}.

	^ styles! !

!methodRemoval: StrikeFont #syntheticWithST80Glyphs!
StrikeFont removeSelector: #syntheticWithST80Glyphs!

!StrikeFont reorganize!
('accessing' ascent baseFont baseFont: baseKern characterToGlyphMap characterToGlyphMap: derivativeFonts descent familyName glyphs maxAscii maxWidth minAscii name name: pointSize pointSize: setGlyphs: subscript superscript widthOf: xTable)
('testing' checkCharacter: isBaseFont isBold isItalic isStruckThrough isUnderlined)
('emphasis' bonk:with: derivativeFont:at: emphasis emphasis: emphasized: isSynthetic isSynthetic: makeBoldGlyphs makeItalicGlyphs makeStruckThroughGlyphs makeUnderlinedGlyphs releaseCachedState reset)
('file in/out' printOn:)
('glyphs' alter:formBlock: ensureCleanBold extendMaxAsciiTo: fillZeroWidthSlots glyphAt: glyphAt:put: makeControlCharsVisible makeCrInvisible makeCrVisible makeLfInvisible makeLfVisible makeTabInvisible makeTabVisible useLeftArrow useRightArrow useUnderscore widen:by:)
('private' createCharacterToGlyphMap fixDerivatives)
('building' buildFromForm:data:name: setGlyphsDepthAtMost: stripHighGlyphs)
('objects from disk' objectForDataStream:)
('character shapes' takeGlyphFor:from:in:)
('synthetic derivatives' boldDerivative syntheticBold syntheticItalic syntheticST80Glyphs syntheticStruckThrough syntheticSubscript syntheticSuperscript syntheticUnderlined)
('displaying' on:displayString:from:to:at:color:)
('copying' postCopy)
!


!AbstractFont reorganize!
('accessing' baseKern characterToGlyphMap derivativeFonts height pointSize xTable)
('measuring' approxWidthOfText: widthOf: widthOfString: widthOfString:from:to: widthOfStringOrText:)
('caching' releaseCachedState)
('derivatives - emphasis' bold boldItalic emphasized: italic struckThrough underlined)
('displaying' on:displayString:from:to:at:color:)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Editor initialize.
SHTextStylerST80 initialize.!


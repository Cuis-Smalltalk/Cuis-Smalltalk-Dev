'From Cuis 4.2 of 25 July 2013 [latest update: #2443] on 31 July 2015 at 1:01:07.814509 am'!

!TextEmphasis commentStamp: 'jmv 7/31/2015 00:59' prior: 0!
A TextEmphasis, encodes a characteristic applicable to all fonts.  The encoding is as follows:
	1	bold
	2	itallic
	4	underlined		
	8	struck out
	16 	withUnderscoreGlyphs!


!AbstractFont methodsFor: 'derivatives - emphasis' stamp: 'jmv 7/30/2015 21:35'!
withUnderscoreGlyphs
	^self emphasized: 16! !


!AbstractFont class methodsFor: 'constants' stamp: 'jmv 7/30/2015 21:35'!
withUnderscoreGlyphsCode
	^16! !


!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 7/30/2015 21:23'!
makeUnderscoreGlyphs
	characterToGlyphMap _ characterToGlyphMap copy.
	self useUnderscore! !


!TextEmphasis class methodsFor: 'instance creation' stamp: 'jmv 7/30/2015 21:35'!
withUnderscoreGlyphs
	^ self new emphasisCode: AbstractFont withUnderscoreGlyphsCode! !


!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 7/29/2015 17:49'!
ansiAssignment
	"#bold"

	^ #(
		ansiAssignment
	)! !

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 7/29/2015 17:49'!
assignment
	"#bold"

	^ #(
		assignment
	)! !

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 7/29/2015 17:49'!
pattern
	"#bold"

	^ #(
		patternKeyword
		patternBinary
		patternUnary
	)! !

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 7/29/2015 23:12'!
return

	^ #(
		#return
	)! !


!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 7/31/2015 01:00'!
emphasis: code 
	"Set the integer code for synthetic bold, itallic, underline, strike-out, and withUnderscoreGlyphs
	where bold=1, italic=2, underlined=4, and struck out=8, withUnderscoreGlyphs=16."

	emphasis _ code! !

!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 7/30/2015 21:26'!
emphasized: code
	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis derivedFrom safeCode |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].

	derivativeFonts ifNil: [derivativeFonts := Array new: 32].
	derivativeFonts size < 32 ifTrue: [
		derivativeFonts := derivativeFonts, (Array new: 32-derivativeFonts size)].

	derivative _ derivativeFonts at: (safeCode _ code min: derivativeFonts size).
	derivative ifNotNil: [^ derivative].  "Already have this style"

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis _ 1 bitShift: safeCode highBit - 1.
	derivedFrom _ self emphasized: safeCode - addedEmphasis.  "Order is Bold, Italic, Underlined, StruckThrough, UnderscoreGlyphs"
	derivative _ derivedFrom copy.
	addedEmphasis = 1 ifTrue: [   "Compute synthetic bold version of the font"
		derivative
			ensureCleanBold;
			name: derivedFrom name , 'B';
			makeBoldGlyphs ].
	addedEmphasis = 2 ifTrue: [   "Compute synthetic italic version of the font"
		derivative
			name: derivedFrom name , 'I';
			makeItalicGlyphs ].
	addedEmphasis = 4 ifTrue: [   "Compute underlined version of the font"
		derivative
			name: derivedFrom name , 'U';
			makeUnderlinedGlyphs ].
	addedEmphasis = 8 ifTrue: [   "Compute struckThrough version of the font"
		derivative
			name: derivedFrom name , 'X';
			makeStruckThroughGlyphs ].
	addedEmphasis = 16 ifTrue: [   "Compute version of the font with Underscore Glyphs"
		derivative
			name: derivedFrom name , 'S';
			makeUnderscoreGlyphs ].
	derivative baseFont: self.
	derivative emphasis: safeCode.
	derivativeFonts at: safeCode put: derivative.
	^ derivative! !


!StringMorph methodsFor: 'font' stamp: 'jmv 7/31/2015 00:59'!
emphasis: aNumber
	"Set the receiver's emphasis as indicated. aNumber is a bitmask with the following format:

	bit	attribute
	1	bold
	2	italic
	4	underlined
	8	struckThrough
	16	withUnderscoreGlyphs
	"

	"examples: 0 -> plain.  
	1 -> bold.  2 -> italic.  3 -> bold italic.  4 -> underlined  
	5 -> bold underlined.  6 -> italic underlined.   7 -> bold italic underlined   
	etc..."

	emphasis _ aNumber.
	^ self font: font emphasis: emphasis! !


!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 7/31/2015 00:04'!
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
		{#unfinishedComment . colors at: #pseudoVariables . self italic}.
		{#comment . colors at: #methodTags . self italic}.
		{#string . colors at: #instVar . #normal}.
		{#literal . nil . self italic}.
		{#incompleteIdentifier . colors at: #tempVars . {#italic. #underlined. #withUnderscoreGlyphs}}.
		{#classVar . colors at: #tempVars . #bold}.
	}.

	^ styles! !

!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 7/29/2015 23:12'!
pseudoVariables

	^ #(
		#self
		#super
		#true
		#false
		#nil
		#thisContext
	)! !

!methodRemoval: Theme #italizeArguments!
Theme removeSelector: #italizeArguments!
!methodRemoval: Theme #nilly!
Theme removeSelector: #nilly!
!methodRemoval: TextEmphasis class #outline!
TextEmphasis class removeSelector: #outline!

!StrikeFont reorganize!
('accessing' ascent baseFont baseFont: baseKern characterToGlyphMap characterToGlyphMap: derivativeFonts descent familyName glyphs maxAscii maxWidth minAscii name name: pointSize pointSize: setGlyphs: subscript superscript widthOf: xTable)
('testing' checkCharacter: isBaseFont isBold isItalic isStruckThrough isUnderlined)
('emphasis' bonk:with: derivativeFont:at: emphasis emphasis: emphasized: isSynthetic isSynthetic: makeBoldGlyphs makeItalicGlyphs makeStruckThroughGlyphs makeUnderlinedGlyphs makeUnderscoreGlyphs releaseCachedState reset)
('file in/out' printOn:)
('glyphs' alter:formBlock: ensureCleanBold extendMaxAsciiTo: fillZeroWidthSlots glyphAt: glyphAt:put: makeControlCharsVisible makeCrInvisible makeCrVisible makeLfInvisible makeLfVisible makeTabInvisible makeTabVisible useLeftArrow useUnderscore widen:by:)
('private' createCharacterToGlyphMap fixDerivatives)
('building' buildFromForm:data:name: setGlyphsDepthAtMost: stripHighGlyphs)
('objects from disk' objectForDataStream:)
('as yet unclassified' on:displayString:from:to:at:color:)
('character shapes' takeGlyphFor:from:in: useLargerPunctuation useRegularPunctuation)
!

!methodRemoval: AbstractFont class #outlineCode!
AbstractFont class removeSelector: #outlineCode!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
SHTextStylerST80 initialize!


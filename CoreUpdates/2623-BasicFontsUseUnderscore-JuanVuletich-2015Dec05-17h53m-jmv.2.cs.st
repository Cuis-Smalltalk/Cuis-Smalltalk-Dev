'From Cuis 4.2 of 25 July 2013 [latest update: #2622] on 5 December 2015 at 6:21:42.041431 pm'!

!TextEmphasis commentStamp: '<historical>' prior: 0!
A TextEmphasis, encodes a characteristic applicable to all fonts.  The encoding is as follows:
	1	bold
	2	itallic
	4	underlined		
	8	struck out!


!TextEmphasis commentStamp: '<historical>' prior: 0!
A TextEmphasis, encodes a characteristic applicable to all fonts.  The encoding is as follows:
	1	bold
	2	itallic
	4	underlined		
	8	struck out!


!StrikeFont methodsFor: 'emphasis' stamp: 'jmv 12/5/2015 18:08'!
emphasized: code
	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis derivedFrom safeCode |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].

	derivativeFonts ifNil: [derivativeFonts := Array new: 16].
	derivativeFonts size < 16 ifTrue: [
		derivativeFonts := derivativeFonts, (Array new: 16-derivativeFonts size)].

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
	derivative baseFont: self.
	derivative emphasis: safeCode.
	derivativeFonts at: safeCode put: derivative.
	^ derivative! !

!StrikeFont methodsFor: 'building' stamp: 'jmv 12/5/2015 18:09'!
buildFromForm: allGlyphs data: data name: aString

	| x |
	pointSize _ data first asNumber.
	ascent _ data second asNumber.
	descent _ data third asNumber.
	
	minAscii _ 28.
	maxAscii _ 255.
	name _ aString.
	type _ 0.  "ignored for now"
	superscript _ ascent - descent // 3.	
	subscript _ descent - ascent // 3.	
	emphasis _ 0.

	xTable _ (Array new: 258) atAllPut: 0.
	maxWidth _ 0.
	glyphs _ allGlyphs depth > 16 ifTrue: [ allGlyphs asFormOfDepth: 16 ] ifFalse: [ allGlyphs ].
	x _ 0.
	minAscii to: maxAscii+1 do: [ :i |
		x _ (data at: i-minAscii+4) asNumber.
		xTable at: i+1 put: x].
	xTable at: 258 put: x.
	self reset.
	derivativeFonts _ nil! !


!FileList methodsFor: 'volume list and pattern' stamp: 'tk 4/7/98 15:26'!
pattern

	^ pattern ifNil: ['*']
! !

!FileList methodsFor: 'private' stamp: 'jmv 12/5/2015 18:04'!
defaultContents
	acceptedContentsCache _ list
			ifNil: [String new]
			ifNotNil: [
				String streamContents: [ :s | 
					s nextPutAll: 'NO FILE SELECTED'; newLine.
					s nextPutAll: '  -- Folder Summary --'; newLine.
					list do: [ :item | s nextPutAll: item; newLine]]].
	brevityState _ #FileList.
	^ acceptedContentsCache! !

!FileList methodsFor: 'private' stamp: 'jmv 12/5/2015 18:07'!
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| fileSize first5000 |

directory // fileName readStream: [ :f |
	f ifNil: [^ 'For some reason, this file cannot be read'].
	(brevityFlag not or: [(fileSize := f size) <= 2000000]) ifTrue: [
		acceptedContentsCache _ f contentsOfEntireFile.
		brevityState := #fullFile.   "don't change till actually read"
		^ acceptedContentsCache ].

	"if brevityFlag is true, don't display long files when first selected"
	first5000 := f next: 5000.
].
	acceptedContentsCache _
'File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 5000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 5000 characters.' format: {fileName. fileSize. first5000}.
	brevityState := #briefFile.   "don't change till actually read"
	^ acceptedContentsCache! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 12/5/2015 18:05'!
changeEmphasis: aKeyboardEvent
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"control 0..9 -> 0..9"
	"This is a user command, and generates undo"

	| keyCode attributeToAdd attributeToRemove oldAttributes |
	keyCode _ ('01234'
		indexOf: aKeyboardEvent keyCharacter
		ifAbsent: [ 1 ]) - 1.
	oldAttributes _ emphasisHere.

	keyCode = 0 ifTrue: [ attributeToAdd _ TextEmphasis normal ].

	(keyCode between: 1 and: 4) ifTrue: [
		attributeToAdd _ TextEmphasis perform: (#(#bold #italic #underlined #struckThrough) at: keyCode).
		oldAttributes do: [ :oldAtt |
			oldAtt = attributeToAdd ifTrue: [
				attributeToAdd _ nil.
				attributeToRemove _ oldAtt ]]].

	attributeToAdd ifNotNil: [ self applyAttribute: attributeToAdd ].
	attributeToRemove ifNotNil: [ self unapplyAttribute: attributeToRemove ].
	^ true! !


!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:19'!
setListFontTo: aFont
	"Set the list font as indicated"

	self parameters at: #standardListFont put: aFont! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 4/20/2015 16:19'!
setWindowTitleFontTo: aFont
	"Set the window-title font to be as indicated"

	self parameters at: #windowTitleFont put: aFont! !


!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 12/5/2015 18:02'!
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
		{#comment . colors at: #methodTags . self italic}.
		{#string . colors at: #instVar . #normal}.
		{#literal . nil . self italic}.
		{#incompleteIdentifier . colors at: #tempVars . {#italic. #underlined}}.
		{#classVar . colors at: #tempVars . #bold}.
	}.

	^ styles! !


!SHTextStylerST80 class methodsFor: 'preferences' stamp: 'jmv 12/5/2015 18:24'!
initializePreferences
	
	(Preferences preferenceAt: #syntaxHighlightingAsYouType) ifNil:[
		Preferences 
			disable: #browseWithPrettyPrint.
		Preferences 
			addPreference: #syntaxHighlightingAsYouType
			 categories: #( browsing)
			default: true 
			balloonHelp: 'Enable, or disable, Shout - Syntax Highlighting As You Type. When enabled, code in Browsers and Workspaces is styled to reveal its syntactic structure. When the code is changed (by typing some characters, for example), the styling is changed so that it remains in sync with the modified code'].
	(Preferences preferenceAt: #syntaxHighlightingAsYouTypeAnsiAssignment) ifNil:[
		Preferences 
			addPreference: #syntaxHighlightingAsYouTypeAnsiAssignment
			 categories: #( browsing)
			default: false 
			balloonHelp: 'If true, and syntaxHighlightingAsYouType is enabled,  all left arrow assignments ( _ ) will be converted to the ANSI format ( := ) when a method is selected in a Browser. Whilst editing a method, this setting has no effect - both the left arrow and the ansi format may be used'.
		(Preferences preferenceAt: #syntaxHighlightingAsYouTypeAnsiAssignment)
			changeInformee: self
			changeSelector: #ansiAssignmentPreferenceChanged].		
	(Preferences preferenceAt: #syntaxHighlightingAsYouTypeLeftArrowAssignment) ifNil:[
		Preferences 
			addPreference: #syntaxHighlightingAsYouTypeLeftArrowAssignment
		 	categories: #( browsing)
			default: false 
			balloonHelp: 'If true, and syntaxHighlightingAsYouType is enabled,  all ANSI format assignments ( := ) will be converted to left arrows ( _ ) when a method is selected in a Browser. Whilst editing a method, this setting has no effect - both the left arrow and the ansi format may be used'.
		(Preferences preferenceAt: #syntaxHighlightingAsYouTypeLeftArrowAssignment)
			changeInformee: self 
			changeSelector: #leftArrowAssignmentPreferenceChanged ].! !

!methodRemoval: TextEmphasis class #withUnderscoreGlyphs!
TextEmphasis class removeSelector: #withUnderscoreGlyphs!
!methodRemoval: String #asTextWithUnderscoreGlyphs!
String removeSelector: #asTextWithUnderscoreGlyphs!
!methodRemoval: FileList #withUnderscoreGlyphsIfNotSmalltalkCode:!
FileList removeSelector: #withUnderscoreGlyphsIfNotSmalltalkCode:!
!methodRemoval: StrikeFont #makeUnderscoreGlyphs!
StrikeFont removeSelector: #makeUnderscoreGlyphs!
!methodRemoval: AbstractFont class #withUnderscoreGlyphsCode!
AbstractFont class removeSelector: #withUnderscoreGlyphsCode!
!methodRemoval: AbstractFont #withUnderscoreGlyphs!
AbstractFont removeSelector: #withUnderscoreGlyphs!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

StrikeFont allInstancesDo: [ :f | f reset ]. Smalltalk garbageCollect.
StrikeFont allInstancesDo: [ :f | f useUnderscore ].
Preferences standardFonts.
Theme current generateShoutConfig.
SHTextStylerST80 initialize.
Smalltalk garbageCollect.!


'From Cuis 4.1 of 12 December 2012 [latest update: #1738] on 25 June 2013 at 11:24:45.749763 pm'!

!Preferences class methodsFor: 'shout' stamp: 'jmv 6/25/2013 17:29'!
italicsInShout
	^ self
		valueOfFlag: #italicsInShout
		ifAbsent: [true]! !


!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 6/25/2013 17:30'!
italic
	^Preferences italicsInShout
		ifTrue: [ #italic ]
		ifFalse: [ #normal ]! !


!StrikeFont class methodsFor: 'removing' stamp: 'jmv 6/25/2013 17:29'!
removeMostFonts
"
StrikeFont removeMostFonts
"
	| familyDict |
	Preferences disable: #italicsInShout.
	SHTextStylerST80 initialize.
	familyDict _ AvailableFonts at: 'DejaVu'.
	familyDict keys do: [ :k |
		(#(8 10 12) includes: k) 
			ifTrue: [
				(familyDict at: k) derivativeFont: nil at: 0 ]
			ifFalse: [
				familyDict removeKey: k ]].
	
	Preferences setDefaultFonts: #(
		(setSystemFontTo: 'DejaVu' 10)
		(setListFontTo: 'DejaVu' 10)
		(setMenuFontTo: 'DejaVu' 10)
		(setWindowTitleFontTo: 'DejaVu' 12)
		(setCodeFontTo: 'DejaVu' 10)
		(setButtonFontTo: 'DejaVu' 10))! !


!GrafPort methodsFor: 'private' stamp: 'jmv 6/25/2013 23:23'!
installStrikeFont: aStrikeFont foregroundColor: foregroundColor

	| targetColor sourceDepth destDepth |
	sourceForm _ aStrikeFont glyphs.
	sourceDepth _ sourceForm depth.
	destDepth _ destForm depth.
	halftoneForm _ nil.	"Don't use fillColor. Use a more powerful ColorMap"
	sourceY _ 0.
	height _ aStrikeFont height.

	sourceDepth = 1 ifTrue: [
		self combinationRule: Form paint.
		"Set up color map for a different source depth (color font)"
		"Uses caching for reasonable efficiency"
		colorMap _ self cachedFontColormapFrom1BitTo: destDepth.
		colorMap at: 1 put: (destForm pixelValueFor: Color transparent).
		colorMap at: 2 put: (destForm pixelValueFor: foregroundColor) ]
	
	ifFalse: [
		"Enable subpixel rendering if requested, but never for translucent text:
		This technique always draws opaque text. This could be added, by using an extra colormap for the rgbMul phase...
		So far, no need arised for doing so."
		(sourceDepth > 8 and: [
			Preferences subPixelRenderFonts and: [ foregroundColor = Color black or: [ 
				Preferences subPixelRenderColorFonts and: [ foregroundColor isOpaque ]]]]) ifTrue: [
			destDepth > 8 ifTrue: [
				"rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
				self combinationRule: 37.		"rgbMul"
				colorMap _ (foregroundColor ~= Color black or: [
						destDepth = 32 and: [ destForm ~~ Display or: [Preferences properDisplayAlphaForFonts] ]]) ifTrue: [
					"rgbMul / rgbAdd IS component alpha blend for any color of text (neat trick, eh!!)"
					"This colorMap is to be used on the second pass with rule 20 (rgbAdd)
					See #displayString:from:to:at:strikeFont:color:kern:"
					"Note: In 32bpp, if we want the correct alpha in the result, we need the second pass, as the destination could have transparent pixels, 
					and we need to add to the alpha channel"
					self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
			ifFalse: [
				self combinationRule: 25.		"Paint"
				targetColor _ foregroundColor = Color black ifFalse: [ foregroundColor ].
				colorMap _ self colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
		ifFalse: [
			"Do not use rule 34 for 16bpp display. TTCFont uses it, but it builds a glyphs cache for each color used!!"
			self combinationRule: (destDepth = 32 ifTrue: [34 "alphaBlendScaled"] ifFalse: [25 "Paint"]).
			colorMap _ self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: false]]! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/25/2013 23:21'!
presumedSentMessages
	| sent |
	"
	In addition to those here, if it is desired to preserve some methods from deletion, see #nominallyUnsent:
	Smalltalk presumedSentMessages
	"

	"The following should be preserved for doIts, etc"
	sent _ IdentitySet new.
	#( rehashWithoutBecome compactSymbolTable
		browseAllSelect:  lastRemoval
		vScrollBarValue: hScrollBarValue: 
		to: removeClassNamed:
		dragon: hilberts: mandala: web test3 factorial tinyBenchmarks benchFib
		newDepth: restoreAfter: zapAllMethods obsoleteClasses
		removeAllUnSentMessages abandonSources removeUnreferencedKeys
		zapOrganization condenseSources condenseChanges browseObsoleteReferences
		subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:
		startTimerEventLoop unusedClasses allClasses
		unimplemented
		reduceCuis
		variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		printSpaceAnalysis:on:) do: [ :sel |
			sent add: sel].
	"The following may be sent by perform: in dispatchOnChar..."
	Editor withAllSubclassesDo: [ :c |
		c shortcuts asSet do: [ :sel | sent add: sel ].
		c cmdShortcuts asSet do: [ :sel | sent add: sel ]].
	#(beReadOnlyBinding beReadWriteBinding) do: [ :sel |
		sent add: sel].
	^ sent! !


!Theme methodsFor: 'private - shout mappings' stamp: 'jmv 6/25/2013 17:31'!
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
		{self argumentTypes . colors at: #arguments . #italicArguments}.
		{self symbols . colors at: #messages . #bold}.
		{self nilly . nil . #bold}. "This one is odd.  --cbr"
		{self tempVars . colors at: #tempVars . self italic }.
		{self blockTemps . colors at: #tempBar . self italic}
	} do: [:style|
		styles addAll:
			(style first
				collect: [ :category | | elements |
					elements _ style asOrderedCollection.
					elements at: 1 put: category.
					elements last = #italicArguments ifTrue: [
						self italizeArguments
							ifTrue: [ elements at: 3 put: self italic ]
							ifFalse: [ elements removeLast ]].
					Array withAll: elements ])].

	"Miscellaneous remainder after factoring out commonality:"
	self flag: #todo. "investigate meaning of nil in this context"
	styles addAll: {
		{#unfinishedString . colors at: #undefined . #normal}.
		{#undefinedIdentifier . colors at: #undefined . #bold}.
		{#unfinishedComment . colors at: #pseudoVariables . self italic}.
		{#comment . colors at: #methodTags . self italic}.
		{#string . colors at: #instVar . #normal}.
		{#literal . nil . self italic}.
		{#incompleteIdentifier . colors at: #tempVars . {#italic. #underlined}}.
		{#classVar . colors at: #tempVars . #bold}.
	}.

	^ styles! !


'From Cuis 6.0 [latest update: #5513] on 25 October 2022 at 5:24:16 pm'!

!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 10/25/2022 17:20:11'!
useUtf8Editors
	"
	Text editors will work on instances of Utf8String and UnicodeCodePoint.
	This supports the whole Unicode Code Point range
	Requires the use of TrueType fonts. StrikeFonts can not be used.
	Requires the use of Utf8Files. ExtendedAscii files can not be used.
	See #useUtf8Files and #useExtendedAsciiFiles.
	See also #useExtendedAsciiEditors
	TextModel useUtf8Editors
	"
	FileIOAccessor useUtf8Files.
	UnicodeContentsHandling _ #useUtf8Editors.
	UISupervisor ui ifNotNil: [ :w | w whenUIinSafeState: [ w fontPreferenceChanged ]].! !


!String methodsFor: 'displaying' stamp: 'jmv 10/25/2022 17:04:19'!
displayOnVectorEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color
	"Double Dispatch"

	^ engine 
		displayString: self
		from: startIndex
		to: stopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		color: color
		scale: aTrueTypeFont pointSize
		preventMirroring: true.! !


!Utf32String methodsFor: 'displaying' stamp: 'jmv 10/25/2022 17:03:50'!
displayOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color
	"Double Dispatch"

	^engine
		displayUtf32: codePoints
		from: startIndex
		to: stopIndex
		at: p
		trueTypeFont: aTrueTypeFont
		color: color.! !

!Utf32String methodsFor: 'displaying' stamp: 'jmv 10/25/2022 17:04:22'!
displayOnVectorEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color
	"Double Dispatch"

	^engine
		displayUtf32: codePoints
		from: startIndex to: stopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		color: color
		scale: aTrueTypeFont pointSize
		preventMirroring: true
! !


!Utf8String methodsFor: 'displaying' stamp: 'jmv 10/25/2022 17:03:56'!
displayOnBitBltCanvasEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color
	"Double Dispatch"

	| byteStartIndex byteStopIndex |
	byteStartIndex _ self byteIndexAt: startIndex.
	byteStopIndex _ (self byteIndexAt: stopIndex+1)-1.
	^engine
		displayUtf8: bytes
		fromByte: byteStartIndex
		toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont
		color: color.! !

!Utf8String methodsFor: 'displaying' stamp: 'jmv 10/25/2022 17:04:25'!
displayOnVectorEngine: engine from: startIndex to: stopIndex at: p font: aTrueTypeFont color: color
	"Double Dispatch"

	| byteStartIndex byteStopIndex |
	byteStartIndex _ self byteIndexAt: startIndex.
	byteStopIndex _ (self byteIndexAt: stopIndex+1)-1.
	^engine
		displayUtf8: bytes
		fromByte: byteStartIndex toByte: byteStopIndex
		at: p
		trueTypeFont: aTrueTypeFont fontDescription
		color: color
		scale: aTrueTypeFont pointSize
		preventMirroring: true
! !


!Text class methodsFor: 'instance creation' stamp: 'jmv 9/22/2022 15:43:25'!
initialFont: aFont string: aString attribute: aTextAttribute
	"Answer an instance of me whose characters are aString.
	Note: use #baseFont. If emphasis is desired, add it separatedly."

	^self
		string: aString
		attributes: {TextFontFamilyAndSize
			familyName: aFont familyName
			pointSize: aFont pointSize.
			aTextAttribute}! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 10/25/2022 17:14:32'!
drawString: s at: pt font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	displayEngine colorMap: nil.
	^s displayOnBitBltCanvasEngine: displayEngine
		from: 1
		to: s size
		at: pt roundedHAFZ
		font: font
		color: aColor! !


!BitBltCanvasEngine methodsFor: 'text' stamp: 'jmv 9/22/2022 15:44:51'!
displayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer position to place next glyph
	Answer nil if nothing was done
	"

	| answer prevRule secondPassMap sourceDepth destDepth |

	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^nil].
	clipWidth = 0 ifTrue: [^nil].

	sourceForm _ nil. "Set it for each glyph"
	sourceX _ 0.
	sourceY _ 0.
	height _ nil. "Set it for each glyph as sourceForm height".
	self setRuleAndMapFor: 32 foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ aTrueTypeFont cachedFormDepth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont.
		combinationRule := prevRule ].
	^answer! !

!BitBltCanvasEngine methodsFor: 'text' stamp: 'jmv 9/22/2022 15:44:57'!
displayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer position to place next glyph
	Answer nil if nothing was done
	"

	| answer prevRule secondPassMap sourceDepth destDepth |

	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^nil].
	clipWidth = 0 ifTrue: [^nil].

	sourceForm _ nil. "Set it for each glyph"
	sourceX _ 0.
	sourceY _ 0.
	height _ nil. "Set it for each glyph as sourceForm height".
	self setRuleAndMapFor: 32 foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDisplayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ aTrueTypeFont cachedFormDepth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTrueTypeFont.
		combinationRule := prevRule ].
	^answer! !

!BitBltCanvasEngine methodsFor: 'text' stamp: 'jmv 9/22/2022 15:46:56'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer position to place next glyph
	Answer nil if nothing was done
	"

	| answer prevRule secondPassMap sourceDepth destDepth |

	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^nil].
	clipWidth = 0 ifTrue: [^nil].

	sourceForm _ nil. "Set it for each glyph"
	sourceX _ 0.
	sourceY _ 0.
	height _ nil. "Set it for each glyph as sourceForm height".
	self setRuleAndMapFor: 32 foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ aTrueTypeFont cachedFormDepth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTrueTypeFont.
		combinationRule := prevRule ].
	^answer! !

!BitBltCanvasEngine methodsFor: 'private' stamp: 'jmv 9/22/2022 15:48:23'!
setRuleAndMapFor: sourceDepth foregroundColor: foregroundColor

	| targetColor destDepth |
	destDepth _ destForm depth.
	halftoneForm _ nil.	"Don't use fillColor. Use a more powerful ColorMap"

	sourceDepth = 1 ifTrue: [
		self combinationRule: Form paint.
		"Set up color map for a different source depth (color font)"
		"Uses caching for reasonable efficiency"
		colorMap _ self cachedFontColormapFrom1BitTo: destDepth.
		colorMap at: 1 put: (destForm pixelValueFor: `Color transparent`).
		colorMap at: 2 put: (destForm pixelValueFor: foregroundColor) ]
	
	ifFalse: [
		"Enable subpixel rendering if requested, but never for translucent text:
		This technique always draws opaque text. This could be added, by using an extra colormap for the rgbMul phase...
		So far, no need arised for doing so."
		(sourceDepth > 8 and: [
			(Preferences at: #subPixelRenderFonts) and: [ foregroundColor = `Color black` or: [ 
				(Preferences at: #subPixelRenderColorFonts) and: [ foregroundColor isOpaque ]]]]) ifTrue: [
			destDepth > 8 ifTrue: [
				"rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
				self combinationRule: 37.		"rgbMul"
				colorMap _ (foregroundColor ~= `Color black` or: [
						destDepth = 32 and: [ destForm ~~ Display or: [Preferences at: #properDisplayAlphaForFonts] ]]) ifTrue: [
					"rgbMul / rgbAdd IS component alpha blend for any color of text (neat trick, eh!!)"
					"This colorMap is to be used on the second pass with rule 20 (rgbAdd)
					See senders"
					"Note: In 32bpp, if we want the correct alpha in the result, we need the second pass, as the destination could have transparent pixels, 
					and we need to add to the alpha channel"
					self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
			ifFalse: [
				self combinationRule: 25.		"Paint"
				targetColor _ foregroundColor = `Color black` ifFalse: [ foregroundColor ].
				colorMap _ self colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
		ifFalse: [
			"Do not use rule 34 for 16bpp display."
			self combinationRule: (destDepth = 32 ifTrue: [34 "alphaBlendScaled"] ifFalse: [25 "Paint"]).
			colorMap _ self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: false]]! !


!TrueTypeFont methodsFor: 'initialization' stamp: 'jmv 9/22/2022 15:49:05'!
baseFont: aFont

	baseFont _ aFont.
	baseFont ifNotNil: [
		derivativeFonts _ nil ]! !

!TrueTypeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 9/22/2022 15:50:20'!
syntheticSubscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ ((family atPointSize: pointSize * 0.68) emphasized: emphasis).
	^ derivative copy

"
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !

!TrueTypeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 9/22/2022 15:50:37'!
syntheticSuperscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ ((family atPointSize: pointSize * 0.68) emphasized: emphasis).
	^ derivative copy

"
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !


!FontFamily class methodsFor: 'ui' stamp: 'jmv 9/22/2022 15:58:53'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	| fontFamily |
	self promptUserFolders ifNotNil: [ :selectedNameOrDirectory |
		fontFamily _ self readIfNeeded: selectedNameOrDirectory.
		fontFamily includesAscii
			ifTrue: [PreferenceSet setDefaultFont: fontFamily familyName ]
			ifFalse: [self inform: 'Selected font does not include ASCII characters. Can not be set as default.' ]].! !


!LabelMorph methodsFor: 'accessing' stamp: 'jmv 9/22/2022 15:52:26'!
font: aFont emphasis: emphasisCode
	font _ aFont.
	emphasis _ emphasisCode.
	self fitContents.
	self redrawNeeded
"
in inspector say,
	 self font: AbstractFont fromUser emphasis: 1
"! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 9/22/2022 15:57:07'!
preferencesMenuOptions

	^`{
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		10.
				#label 			-> 		'Focus follows mouse'.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		20.
				#label 			-> 		'Click to focus'.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		40.
				#label 			-> 		'Size of GUI elements...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		50.
				#label			->		'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#promptUserAndSetDefault.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current system font family.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		60.
				#label			->		'Load all TrueType Fonts'.
				#object 			-> 		TrueTypeFontFamily.
				#selector 			-> 		#readAdditionalFonts.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		70.
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		10.
				#itemOrder 			-> 		80.
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		10.
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		20.
				#itemOrder 			-> 		20.
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		10.
				#label 			-> 		'Full screen on'.
				#selector 			-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		30.
				#itemOrder 			-> 		20.
				#label 			-> 		'Full screen off'.
				#selector 			-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		10.
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 			-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#submenuOf  			-> 		TheWorldMenu preferencesLabel.
				#itemGroup 			-> 		40.
				#itemOrder 			-> 		20.
				#label 			-> 		'All preferences...'.
				#object 			-> 		PreferenceSet.
				#selector 			-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}`	
	! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 10/25/2022 17:15:26'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| p1 |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ currentTransformation transform: aPoint roundedHAFZ.
	p1 _ p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 _ p1 roundedHAFZ.
	engine colorMap: nil.
	^aString displayOnBitBltCanvasEngine: engine
		from: firstIndex
		to: lastIndex
		at: p1
		font: font
		color: aColor! !


!TTFontDescription methodsFor: 'emphasis' stamp: 'jmv 9/22/2022 15:52:52'!
emphasis
	"See senders of emphasis:"
	| emphasis |
	familyName = fullName ifTrue: [ ^ 0 ].
	emphasis := 0.
	('*Bold*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 1 ].
	('*Oblique*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 2 ].
	('*Italic*' match: subfamilyName) ifTrue: [ emphasis := emphasis bitOr: 2 ].
	^ emphasis! !

!methodRemoval: FontFamily class #readAdditionalTrueTypeFonts stamp: 'jmv 10/25/2022 17:09:53'!
FontFamily class removeSelector: #readAdditionalTrueTypeFonts!
!methodRemoval: TrueTypeFont #onBitBltCanvasEngine:displayString:from:to:at:color: stamp: 'jmv 10/25/2022 17:16:27'!
TrueTypeFont removeSelector: #onBitBltCanvasEngine:displayString:from:to:at:color:!
!methodRemoval: AbstractFont #onBitBltCanvasEngine:displayString:from:to:at:color: stamp: 'jmv 10/25/2022 17:16:00'!
AbstractFont removeSelector: #onBitBltCanvasEngine:displayString:from:to:at:color:!

!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: size)
('comparing' < <= = > >= caseInsensitiveLessOrEqual: crc16 hash is:substringAt: sameAs:)
('copying' , appendToString:)
('converting' asByteString asHex asLowercase asUppercase asUtf32Words asUtf32Words: asUtf8Bytes asUtf8Bytes: asUtf8BytesOrByteString capitalized fixLineEndings substrings surroundedBySingleQuotes thatCanHoldCodePoints translateFrom:to:table: translateToLowercase translateToUppercase translateWith: uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('private' replaceFrom:to:with:startingAt:)
('system primitives' numArgs)
('testing' canHoldCodePoints hasContentsInExplorer is: isAscii isByteString isEmpty)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('displaying' displayOnBitBltCanvasEngine:from:to:at:font:color: displayOnVectorEngine:from:to:at:font:color:)
!


!Utf32String reorganize!
('accessing' at: isLastLineSeparator isLastSeparator size words words:)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with:)
('comparing' = hash sameAs:)
('converting' asUtf32String asUtf32Words asUtf8Bytes)
('testing' isAscii isEmpty isUtf32String)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('initialization' initialize)
('displaying' displayOnBitBltCanvasEngine:from:to:at:font:color: displayOnVectorEngine:from:to:at:font:color:)
!


!Utf8String reorganize!
('accessing' at: bytes findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: isLastLineSeparator isLastSeparator lineIndicesDo: size)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with: copyWith: shallowCopy)
('comparing' < <= = > >= basicHash caseInsensitiveLessOrEqual: computeHash hash is:substringAt: sameAs:)
('converting' asByteString asHex asUtf32Words asUtf8Bytes asUtf8String reversed substrings surroundedBySingleQuotes thatCanBeModified withLineEndings:)
('enumerating' do: from:to:do: from:to:doWithUtf8Bytes:)
('testing' isAscii isEmpty isUtf8String)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('printing' isLiteral)
('private' boundsCheck: byteIndexAt: codePointIndexAt: computeCountAndIndex computeCountAndIndexFull computeCountNoIndex encodedByteCountAt: privateBytes:codePointCount: stride)
('as yet unclassified' numArgs)
('initialization' initialize)
('displaying' displayOnBitBltCanvasEngine:from:to:at:font:color: displayOnVectorEngine:from:to:at:font:color:)
!


!AbstractFont reorganize!
('accessing' ascent baseKern characterToGlyphMap pointSize xTable)
('measuring' approxWidthOfText: normalizedWidthOf: widthOf: widthOfString: widthOfString:from:to: widthOfStringOrText:)
('caching' releaseCachedState)
('derivatives - emphasis' bold boldItalic emphasized: italic struckThrough underlined)
!


!TrueTypeFont reorganize!
('printing' printOn:)
('objects from disk' objectForDataStream:)
('initialization' addDerivativeFont: baseFont: family:ttFontDescription:pointSize:)
('accessing' ascent baseFont cachedFormDepth descent familyName fontDescription lineGap lineSpacing pointSize widthOf: widthOfCodePoint: widthOfUtf8Byte1:b2:b3:b4:)
('testing' isBaseFont isBold isItalic isStruckThrough isSubscript isSuperscript isUnderlined)
('glyph access' glyphAt: glyphAtUtf8Byte1:byte2:byte3:byte4:)
('emphasis' addEmphasis: emphasis emphasized:)
('synthetic derivatives' makeStruckThroughGlyph: makeUnderlinedGlyph: syntheticSubscript syntheticSuperscript)
('swapping glyphs' useShortUnderscore)
('as yet unclassified' releaseCachedState)
!


!FontFamily class reorganize!
('accessing' availableFamilies defaultFamilyName defaultPointSize familyNames)
('initialization' addFamily: defaultFamilyName: defaultFamilyName:defaultPointSize: defaultPointSize:)
('accessing fonts' defaultFamilyAndPointSize defaultFamilyPointSize: defaultOrTrueTypeFamilyAndPointSize familyName:pointSize: familyNamed: pointSizesFor:)
('cached state access' releaseClassCachedState)
('defaults' defaultLineSpacing)
('ui' promptUser promptUserAlt promptUserAndSetDefault promptUserFolders readIfNeeded:)
!


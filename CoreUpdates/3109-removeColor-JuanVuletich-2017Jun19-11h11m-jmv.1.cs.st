'From Cuis 5.0 of 7 November 2016 [latest update: #3103] on 19 June 2017 at 11:15:55 am'!
!methodRemoval: TranslucentColour #setRed:green:blue:alpha:colorSpace:!
TranslucentColour removeSelector: #setRed:green:blue:alpha:colorSpace:!
!methodRemoval: Colour #floatRGB!
Colour removeSelector: #floatRGB!
!methodRemoval: Colour #setRed:green:blue:colorSpace:!
Colour removeSelector: #setRed:green:blue:colorSpace:!

!Colour reorganize!
('access' alpha blue brightness chroma green hue icon iconOrThumbnailOfSize: luminance red saturation swatch)
('conversions' asNontranslucentColor bitPatternForDepth: bitPatternForGrayForm closestPixelValue1 closestPixelValue2 closestPixelValue4 closestPixelValue8 dominantColor indexInMap: makeForegroundColor pixelValueForDepth: pixelWordFor:filledWith: pixelWordForDepth:)
('equality' diff: rgbDistance:)
('groups of shades' darkShades: lightShades: mix:shades: wheel:)
('named colors' closestAssocFrom: closestColor closestColorAssociation closestColorFrom: closestColorName closestNameFrom:)
('other' colorName name)
('printing' hexStringRGB printOn: printString storeArrayOn: storeArrayValuesOn: storeOn:)
('queries' isBlack isOpaque isTransparent isWhite)
('selection' isBlue isBlueGreen isBright isBrown isCyan isDark isGray isGreen isGrey isLight isMagenta isOrange isPastel isPink isRed isSaturated isYellow)
('testing' is: mightBeTranslucent)
('transformations' * + - / adjustBrightness: adjustSaturation:brightness: alpha: alphaMixed:with: atLeastAsLuminentAs: atMostAsLuminentAs: blacker dansDarker darker duller lighter mixed:with: muchDarker muchLighter negated orColorUnlike: paler quiteBlacker quiteWhiter slightlyDarker slightlyLighter slightlyWhiter twiceDarker twiceLighter veryMuchDarker veryMuchLighter whiter)
('private' attemptToMutateError basicSetRed:green:blue: setHue:chroma:brightness: setHue:chroma:luminance: setHue:saturation:brightness: setRed:green:blue: setRed:green:blue:range:)
('as yet unclassified' color)
!


!TranslucentColour reorganize!
('accessing' alpha)
('printing' storeArrayValuesOn: storeOn:)
('conversions' alpha: asNontranslucentColor bitPatternForDepth: pixelValueForDepth: pixelWordForDepth:)
('private' setRed:green:blue:alpha:)
('queries' isOpaque isTransparent)
!

!classRemoval: #Color!
Smalltalk removeClassNamed: #Color!
!classRemoval: #TranslucentColor!
Smalltalk removeClassNamed: #TranslucentColor!

'From Cuis 5.0 [latest update: #3888] on 29 September 2019 at 1:09:24 pm'!

!ArrayedCollection methodsFor: 'private' stamp: 'jmv 9/29/2019 13:00:32'!
species
	"For methods such as #select:, and for instances of fixed size classes, such as Color,
	find an appropriate class for new instances."
	| candidate |
	candidate _ self class.
	[ candidate instancesMustBeOfSpecificSize ] whileTrue: [
		candidate _ candidate superclass ].
	^candidate! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 9/29/2019 12:49:41'!
numElements
	^3! !


!TranslucentColor class methodsFor: 'instance creation' stamp: 'jmv 9/29/2019 12:49:48'!
numElements
	^4! !


!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 9/29/2019 12:52:40'!
new
	"Answer a new instance of me, with size = 0, unles a specific size is used."

	^self new: self numElements! !

!methodRemoval: TranslucentColor class #new!
TranslucentColor class removeSelector: #new!
!methodRemoval: Color class #new!
Color class removeSelector: #new!
!methodRemoval: Float64Array class #new!
Float64Array class removeSelector: #new!

!Color reorganize!
('access' alpha blue brightness chroma color green hue icon iconOrThumbnailOfSize: luminance red saturation swatch)
('conversions' asNontranslucentColor bitPatternForDepth: bitPatternForGrayForm closestPixelValue1 closestPixelValue2 closestPixelValue4 closestPixelValue8 dominantColor indexInMap: makeForegroundColor pixelValueForDepth: pixelWordFor:filledWith: pixelWordForDepth:)
('equality' diff: rgbDistance:)
('groups of shades' darkShades: lightShades: mix:shades: wheel:)
('other' colorName name)
('printing' hexStringRGB printOn: printString storeArrayOn: storeArrayValuesOn: storeOn:)
('queries' isBlack isOpaque isTransparent isWhite)
('selection' isBlue isCyan isGreen isMagenta isRed isYellow)
('testing' is: isCollection mightBeTranslucent)
('transformations' * *= + += - -= / /= adjustBrightness: adjustSaturation:brightness: alpha: alphaMixed:with: atLeastAsLuminentAs: atMostAsLuminentAs: blacker dansDarker darker duller lighter mixed:with: muchDarker muchLighter negated orColorUnlike: paler quiteBlacker quiteWhiter slightlyDarker slightlyLighter slightlyWhiter twiceDarker twiceLighter veryMuchDarker veryMuchLighter whiter)
('private' attemptToMutateError basicSetRed:green:blue: clipToValidValues setHue:chroma:brightness: setHue:chroma:luminance: setHue:saturation:brightness: setRed:green:blue: setRed:green:blue:range:)
('object serialization' convertToCurrentVersion:refStream:)
!


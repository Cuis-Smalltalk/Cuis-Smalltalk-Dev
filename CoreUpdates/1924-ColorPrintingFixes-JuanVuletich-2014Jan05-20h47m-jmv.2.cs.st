'From Cuis 4.2 of 25 July 2013 [latest update: #1922] on 5 January 2014 at 9:18:19.300666 pm'!

!Color methodsFor: 'printing' stamp: 'jmv 1/5/2014 21:04'!
storeArrayValuesOn: aStream


	self red printOn: aStream fractionDigits: 3.
	aStream space.
	self green printOn: aStream fractionDigits: 3.
	aStream space.
	self blue printOn: aStream fractionDigits: 3

! !

!Color methodsFor: 'printing' stamp: 'jmv 1/5/2014 21:01'!
storeOn: aStream

	aStream
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll: ' r: '.
	self red printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' g: '.
	self green printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' b: '.
	self blue printOn: aStream fractionDigits: 3.
	aStream nextPut: $)! !


!Color class methodsFor: 'color from user' stamp: 'jmv 1/5/2014 21:16'!
fromUser
	"Displays a color palette of colors, waits for a mouse click, and returns the selected color. Any pixel on the Display can be chosen, not just those in the color palette."
	"Note: Since the color chart is cached, you may need to do 'ColorChart _ nil' after changing the oldColorPaletteForDepth:extent: method."
	"Color fromUser"

	| d startPt save tr oldColor c here s |
	d _ Display depth.
	((ColorChart == nil) or: [ColorChart depth ~= Display depth]) 
		ifTrue: [ColorChart _ self oldColorPaletteForDepth: d extent: (2 * 144)@80].
	Sensor mousePoint y < Display center y 
		ifTrue: [startPt _ 0@(Display boundingBox bottom - ColorChart height)]
		ifFalse: [startPt _ 0@0].

	save _ Form fromDisplay: (startPt extent: ColorChart extent).
	ColorChart displayAt: startPt.
	tr _ ColorChart extent - (50@19) corner: ColorChart extent.
	tr _ tr translatedBy: startPt.

	oldColor _ nil.
	[Sensor isAnyButtonPressed] whileFalse: [
		c _ Display colorAt: (here _ Sensor mousePoint).
		(tr containsPoint: here)
			ifFalse: [Display fill: (0@61+startPt extent: 20@19) fillColor: c]
			ifTrue: [
				c _ Color transparent.
				Display fill: (0@61+startPt extent: 20@19) fillColor: Color white].
		c = oldColor ifFalse: [
			Display fillWhite: (20@61 + startPt extent: 135@19).
			s _ c isTransparent
				ifTrue: [ 'transparent']
				ifFalse: [
					String streamContents: [ :strm |
						c storeArrayValuesOn: strm ]].
			s displayAt: 20@61 + startPt.
			oldColor _ c]].
	save displayAt: startPt.
	Sensor waitNoButton.
	^ c! !


!TranslucentColor methodsFor: 'printing' stamp: 'jmv 1/5/2014 21:06'!
storeArrayValuesOn: aStream

	self isTransparent ifTrue: [
		^ aStream space].
	super storeArrayValuesOn: aStream.
	aStream space.
	self alpha printOn: aStream fractionDigits: 3! !

!TranslucentColor methodsFor: 'printing' stamp: 'jmv 1/5/2014 21:20'!
storeOn: aStream

	self isTransparent ifTrue: [^ aStream nextPutAll: '(Color transparent)'].
	aStream
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll: ' r: '.
	self red printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' g: '.
	self green printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' b: '.
	self blue printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' alpha: '.
	self alpha printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ')'! !

!methodRemoval: Color #shortPrintOn:!
Color removeSelector: #shortPrintOn:!
!methodRemoval: Color #shortPrintString!
Color removeSelector: #shortPrintString!

!Color reorganize!
('access' alpha blue brightness chroma green hue icon iconOrThumbnailOfSize: luminance red saturation swatch)
('conversions' asNontranslucentColor bitPatternForDepth: closestPixelValue1 closestPixelValue2 closestPixelValue4 closestPixelValue8 dominantColor indexInMap: makeForegroundColor pixelValueForDepth: pixelWordFor:filledWith: pixelWordForDepth:)
('equality' = diff: hash rgbDistance:)
('groups of shades' darkShades: lightShades: mix:shades: wheel:)
('named colors' closestAssocFrom: closestColor closestColorAssociation closestColorFrom: closestColorName closestNameFrom:)
('objects from disk' objectForDataStream:)
('other' display name)
('printing' printOn: storeArrayOn: storeArrayValuesOn: storeOn:)
('queries' isBlack isOpaque isTransparent isWhite)
('selection' isBlue isBlueGreen isBright isBrown isCyan isDark isGray isGreen isGrey isLight isMagenta isOrange isPastel isPink isRed isSaturated isYellow)
('testing' is: mightBeTranslucent)
('transformations' * + - / adjustBrightness: adjustSaturation:brightness: alpha: alphaMixed:with: atLeastAsLuminentAs: atMostAsLuminentAs: blacker dansDarker darker duller lighter mixed:with: muchDarker muchLighter negated orColorUnlike: paler quiteBlacker quiteWhiter slightlyDarker slightlyLighter slightlyWhiter twiceDarker twiceLighter veryMuchDarker veryMuchLighter whiter)
('private' attemptToMutateError fix floatRGB flushCache setHue:chroma:brightness: setHue:chroma:luminance: setHue:saturation:brightness: setRed:green:blue:colorSpace: setRed:green:blue:range:colorSpace:)
!


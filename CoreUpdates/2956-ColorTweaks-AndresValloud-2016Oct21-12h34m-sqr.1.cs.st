'From Cuis 4.2 of 25 July 2013 [latest update: #2953] on 21 October 2016 at 12:48:25 pm'!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace '
	classVariableNames: 'CachedColormaps ColorNamesDict FromGrayColormaps GrayToIndexMap IndexedColors MaskingMap ToGrayColormaps '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
!classDefinition: #TranslucentColor category: #'Graphics-Primitives'!
Color subclass: #TranslucentColor
	instanceVariableNames: 'alpha floatAlpha '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color methodsFor: 'private' stamp: 'sqr 10/15/2016 20:25:37'!
basicSetRed: r green: g blue: b colorSpace: aSymbol
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	floatRGB ifNotNil: [self attemptToMutateError].
	floatRGB _ FloatArray with: r with: g with: b.
	colorSpace _ aSymbol.
	cachedDepth _ nil.
	cachedBitPattern _ nil! !


!TranslucentColor methodsFor: 'private' stamp: 'sqr 10/15/2016 20:29:13'!
setRed: r green: g blue: b colorSpace: aSymbol

	self setRed: r green: g blue: b alpha: 0.0 colorSpace: aSymbol! !


!TranslucentColor class methodsFor: 'instance creation' stamp: 'sqr 10/21/2016 12:44:16'!
r: r g: g b: b alpha: alphaValue
	"Return a color with the given r, g, and b components in the range [0.0..1.0]."

	^ self basicNew
		setRed: r
		green: g
		blue: b
		alpha: alphaValue
		colorSpace: #sRGB! !


!Color methodsFor: 'access' stamp: 'sqr 10/15/2016 20:46:17'!
hue
	"Return the hue of this color, an angle in the range [0.0..360.0].
	Color orange hue
	"

	| r g b max min span h |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ floatRGB max.
	min _ floatRGB min.
	span _ (max - min) asFloat.
	span = 0.0 ifTrue: [ ^ 0.0 ].

	r = max ifTrue: [
		h _ ((g - b) asFloat / span) * 60.0.
	] ifFalse: [
		g = max
			ifTrue: [ h _ 120.0 + (((b - r) asFloat / span) * 60.0). ]
			ifFalse: [ h _ 240.0 + (((r - g) asFloat / span) * 60.0). ].
	].

	^h mod: 360.0! !

!Color methodsFor: 'private' stamp: 'sqr 10/15/2016 20:26:04'!
setRed: r green: g blue: b colorSpace: aSymbol

	self basicSetRed: r green: g blue: b colorSpace: aSymbol! !

!Color methodsFor: 'private' stamp: 'sqr 10/15/2016 20:26:51'!
setRed: r green: g blue: b range: range colorSpace: aSymbol
	"Initialize this color's r, g, and b components to the given values in the range [0..r]."

	self basicSetRed: r green: g blue: b colorSpace: aSymbol.
	floatRGB /= range! !


!Color class methodsFor: 'instance creation' stamp: 'sqr 10/15/2016 20:41:04'!
fromArray: colorDef

	| answer |
	colorDef size = 0 ifTrue: [^self transparent].
	colorDef size between: 3 and: 4 :: ifFalse: [self error: 'Undefined color definition'].
	answer _ self r: (colorDef at: 1) g: (colorDef at: 2) b: (colorDef at: 3).
	^colorDef size = 3
		ifTrue: [answer]
		ifFalse: [answer alpha: (colorDef at: 4)]! !

!Color class methodsFor: 'color name lookup' stamp: 'sqr 10/21/2016 12:44:48'!
traditionalColorNamesDictionary
	"Answer a dictionary of Squeak traditional name->color associations.."

	| nameDict |
	nameDict _ Dictionary new.
	nameDict at: #black put: (Color r: 0 g: 0 b: 0).
	nameDict at: #veryVeryDarkGray put: (Color r: 0.125 g: 0.125 b: 0.125).
	nameDict at: #veryDarkGray put: (Color r: 0.25 g: 0.25 b: 0.25).
	nameDict at: #darkGray put: (Color r: 0.375 g: 0.375 b: 0.375).
	nameDict at: #gray put: (Color r: 0.5 g: 0.5 b: 0.5).
	nameDict at: #lightGray put: (Color r: 0.625 g: 0.625 b: 0.625).
	nameDict at: #veryLightGray put: (Color r: 0.75 g: 0.75 b: 0.75).
	nameDict at: #veryVeryLightGray put: (Color r: 0.875 g: 0.875 b: 0.875).
	nameDict at: #white put: (Color r: 1.0 g: 1.0 b: 1.0).
	nameDict at: #red put: (Color r: 1.0 g: 0 b: 0).
	nameDict at: #yellow put: (Color r: 1.0 g: 1.0 b: 0).
	nameDict at: #green put: (Color r: 0 g: 1.0 b: 0).
	nameDict at: #cyan put: (Color r: 0 g: 1.0 b: 1.0).
	nameDict at: #blue put: (Color r: 0 g: 0 b: 1.0).
	nameDict at: #magenta put: (Color r: 1.0 g: 0 b: 1.0).
	nameDict at: #brown put: (Color r: 0.6 g: 0.2 b: 0).
	nameDict at: #orange put: (Color r: 1.0 g: 0.6 b: 0).
	nameDict at: #lightRed put: (Color r: 1.0 g: 0.8 b: 0.8).
	nameDict at: #lightYellow put: (Color r: 1.0 g: 1.0 b: 0.8).
	nameDict at: #lightGreen put: (Color r: 0.8 g: 1.0 b: 0.6).
	nameDict at: #lightCyan put: (Color r: 0.4 g: 1.0 b: 1.0).
	nameDict at: #lightBlue put: (Color r: 0.8 g: 1.0 b: 1.0).
	nameDict at: #lightMagenta put: (Color r: 1.0 g: 0.8 b: 1.0).
	nameDict at: #lightBrown put: (Color r: 1.0 g: 0.6 b: 0.2).
	nameDict at: #lightOrange put: (Color r: 1.0 g: 0.8 b: 0.4).
	nameDict at: #transparent put: (TranslucentColor r: 0.0 g: 0.0 b: 0.0 alpha: 0.0).
	
	^nameDict
! !


!TranslucentColor methodsFor: 'private' stamp: 'sqr 10/15/2016 20:28:20'!
setRed: r green: g blue: b alpha: alphaValue colorSpace: aSymbol

	self basicSetRed: r green: g blue: b colorSpace: aSymbol.
	floatAlpha _ alphaValue! !


!PasteUpMorph methodsFor: 'drawing' stamp: 'sqr 10/15/2016 20:16:26'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			aCanvas image: backgroundImage at: 0@0 ]
		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [ aCanvas drawsOnDisplay ] and: [ color mightBeTranslucent ])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Cuis Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: 0@0 in: nil
						fillColor: color rule: Form over.
					Display forceToScreen]
				ifFalse: [ super drawOn: aCanvas ]]! !

!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'cachedDepth cachedBitPattern floatRGB colorSpace'
	classVariableNames: 'CachedColormaps ColorNamesDict FromGrayColormaps GrayToIndexMap IndexedColors MaskingMap ToGrayColormaps'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
!classDefinition: #TranslucentColor category: #'Graphics-Primitives'!
Color subclass: #TranslucentColor
	instanceVariableNames: 'floatAlpha'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!TranslucentColor class reorganize!
('instance creation' r:g:b:alpha:)
!


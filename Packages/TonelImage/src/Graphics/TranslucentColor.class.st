"
A TranslucentColor behaves just like a normal color, except that it will pack its alpha value into the fourth position.  This allows creating forms with translucency for use with the alpha blend function of BitBlt.  An alpha of zero is transparent, and 1.0 is opaque.
"
Class {
	#name : 'TranslucentColor',
	#superclass : 'Color',
	#category : 'Graphics-Primitives',
	#package : 'Graphics',
	#tag : 'Primitives'
}

{ #category : 'instance creation' }
TranslucentColor class >> numSlots [
	^4
]

{ #category : 'instance creation' }
TranslucentColor class >> r: r g: g b: b alpha: alphaValue [
	"Return a color with the given r, g, and b components in the range [0.0..1.0]."

	^ self new
		setRed: r
		green: g
		blue: b
		alpha: alphaValue
]

{ #category : 'accessing' }
TranslucentColor >> alpha [
	"Return my alpha value, a number between 0.0 and 1.0 where 0.0 is completely transparent and 1.0 is completely opaque."

	^ self float32SlotAt: 4
]

{ #category : 'conversions' }
TranslucentColor >> alpha: alphaValue [
	"Return a new TranslucentColor with the given amount of opacity ('alpha')."
	alphaValue = 1.0 ifTrue: [
		^ Color new
			setRed: self red
			green: self green
			blue: self blue].
	^ super alpha: alphaValue
]

{ #category : 'conversions' }
TranslucentColor >> asNontranslucentColor [
	^ self alpha: 1.0
]

{ #category : 'conversions' }
TranslucentColor >> bitPatternForDepth: depth [
	"Return an appropriate bit pattern. This will almost never be meaningful for tranlucentColors, except for the degenerate case of tranparency."

	self isTransparent ifTrue: [
		^ Bitmap with: 0].
	^ super bitPatternForDepth: depth
]

{ #category : 'queries' }
TranslucentColor >> isOpaque [

	^self alpha = 1.0
]

{ #category : 'queries' }
TranslucentColor >> isTransparent [
	^ self alpha = 0.0
]

{ #category : 'conversions' }
TranslucentColor >> pixelValueForDepth: d [
	"Return the pixel value for this color at the given depth. Translucency only works in RGB; this color will appear either opaque or transparent at all other depths."
	| basicPixelWord |
	"In 32bpp, if alpha = 0, ignore any color components, and answer R=0, G=0, B=0, Alpha=0.
	In depths > 8 and < 32, pixelValue zero is the special value used by BitBlt to denote transparent."
	self isTransparent ifTrue: [
		^ 0].
	basicPixelWord := super pixelValueForDepth: d.
	^d < 32
		ifTrue: [ basicPixelWord ]
		ifFalse: [ (basicPixelWord bitAnd: 16rFFFFFF) bitOr:
			((self alpha*255.0) rounded
				bitShift: 24) ]
]

{ #category : 'conversions' }
TranslucentColor >> pixelWordForDepth: depth [
	"Return the pixel value for this color at the given depth. Translucency only works in RGB; this color will appear either opaque or transparent at all other depths."

	| basicPixelWord |
	self isTransparent ifTrue: [^ 0].
	basicPixelWord := super pixelWordForDepth: depth.
	^depth < 32
		ifTrue: [ basicPixelWord ]
		ifFalse: [ (basicPixelWord bitAnd: 16rFFFFFF) bitOr:
			((self alpha*255.0) rounded
				bitShift: 24) ]
]

{ #category : 'private' }
TranslucentColor >> setRed: r green: g blue: b alpha: alphaValue [

	self basicSetRed: r green: g blue: b.
	self float32SlotAt: 4 put: alphaValue
]

{ #category : 'printing' }
TranslucentColor >> storeArrayValuesOn: aStream [

	self isTransparent ifTrue: [
		^ aStream space].
	super storeArrayValuesOn: aStream.
	aStream space.
	self alpha printOn: aStream fractionDigits: 3
]

{ #category : 'printing' }
TranslucentColor >> storeOn: aStream [

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
	aStream nextPutAll: ')'
]

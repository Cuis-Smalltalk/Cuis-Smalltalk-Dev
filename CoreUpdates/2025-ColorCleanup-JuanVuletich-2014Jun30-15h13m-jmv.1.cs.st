'From Cuis 4.2 of 25 July 2013 [latest update: #2023] on 30 June 2014 at 3:27:06.668116 pm'!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace '
	classVariableNames: 'CachedColormaps ColorChart ColorNamesDict GrayToIndexMap IndexedColors MaskingMap PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow RandomStream '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color class methodsFor: 'examples' stamp: 'jmv 6/30/2014 15:15'!
experimentsTowarsANewColorPalette
"
self experimentsTowarsANewColorPalette
"
| selectedHue selectedSaturation selectedV selectedColor h s v color width height selectedChroma selectedLuminance |
width _ 300.
height _ 120.
selectedColor _ Color random.
selectedHue _ selectedColor hue.
selectedSaturation _ selectedColor saturation.
selectedChroma _ selectedColor chroma.
selectedV _ selectedColor brightness.
selectedLuminance _ selectedColor luminance.
Display getCanvas fillRectangle: (0@0 extent: height@height) color: selectedColor.
0 to: height do: [ :y |
	v _ 1.0 - (y / height).
	0 to: height do: [ :x |
		s _ x / height.
		color _ Color basicNew setHue: selectedHue saturation: s brightness: v.
		Display colorAt: x@(y+height) put: color
	].
	Display forceToScreen
].
0 to: height do: [ :y | | c |
	v _ 1.0 - (y / height).
	s _ 1.0 - (y / height).
	c _ s.
	0 to: width do: [ :x |
		h _ x / width * 360.
		
		color _ Color basicNew setHue: h chroma: c luminance: selectedLuminance.
"		color _ Color basicNew setHue: h chroma: c brightness: selectedV."
		color ifNil: [ color _ Color black ].
"		color _ Color basicNew setHue: h saturation: s brightness: selectedV."
		Display colorAt: x+height@y put: color.
		
		color _ Color basicNew setHue: h chroma: selectedChroma luminance: v.
"		color _ Color basicNew setHue: h chroma: selectedChroma brightness: v."
		color ifNil: [ color _ Color black ].
"		color _ Color basicNew setHue: h saturation: selectedSaturation brightness: v."
		Display colorAt: x+height@(y+height) put: color.
		
	].
	Display forceToScreen
].! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:24'!
paleGreen

	^ ColorNamesDict at: #paleGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:26'!
tan

	^ ColorNamesDict at: #tan! !

!Color class methodsFor: 'other' stamp: 'jmv 6/30/2014 15:18'!
shutDown
	"Color shutDown"

	CachedColormaps _ nil.	"Maps to translate between color depths"
	MaskingMap _ nil.		"Maps all colors except transparent to black for creating a mask"
! !

!Color class methodsFor: 'color name lookup' stamp: 'jmv 6/30/2014 15:22'!
xkcdFirst48ColorNamesDictionary
	"XKCD color names are based on a survey oc colors people call by the same name.
	 http://blog.xkcd.com/2010/05/03/color-survey-results/"

	"Answer a dictionary of the most agreed upon first 48 xkcd colors"
"
	Color xkcdFirst48ColorNamesDictionary explore.
"	
	| xkcdDict |
	xkcdDict := Dictionary new
	at: #lightPink put: (Color fromString: '#ffd1df') ;
	at: #mustard put: (Color fromString: '#ceb301') ;
	at: #indigo put: (Color fromString: '#380282') ;
	at: #lime put: (Color fromString: '#aaff32') ;
	at: #seaGreen put: (Color fromString: '#53fca1') ;
	at: #periwinkle put: (Color fromString: '#8e82fe') ;
	at: #darkPink put: (Color fromString: '#cb416b') ;
	at: #oliveGreen put: (Color fromString: '#677a04') ;
	at: #peach put: (Color fromString: '#ffb07c') ;
	at: #paleGreen put: (Color fromString: '#c7fdb5') ;
	at: #lightBrown put: (Color fromString: '#ad8150') ;
	at: #hotPink put: (Color fromString: '#ff028d') ;
	at: #black put: (Color fromString: '#000000') ;
	at: #lilac put: (Color fromString: '#cea2fd') ;
	at: #navyBlue put: (Color fromString: '#001146') ;
	at: #royalBlue put: (Color fromString: '#0504aa') ;
	at: #beige put: (Color fromString: '#e6daa6') ;
	at: #salmon put: (Color fromString: '#ff796c') ;
	at: #olive put: (Color fromString: '#6e750e') ;
	at: #maroon put: (Color fromString: '#650021') ;
	at: #brightGreen put: (Color fromString: '#01ff07') ;
	at: #darkPurple put: (Color fromString: '#35063e') ;
	at: #mauve put: (Color fromString: '#ae7181') ;
	at: #forestGreen put: (Color fromString: '#06470c') ;
	at: #aqua put: (Color fromString: '#13eac9') ;
	at: #cyan put: (Color fromString: '#00ffff') ;
	at: #tan put: (Color fromString: '#d1b26f') ;
	at: #darkBlue put: (Color fromString: '#00035b') ;
	at: #lavender put: (Color fromString: '#c79fef') ;
	at: #turquoise put: (Color fromString: '#06c2ac') ;
	at: #darkGreen put: (Color fromString: '#033500') ;
	at: #violet put: (Color fromString: '#9a0eea') ;
	at: #lightPurple put: (Color fromString: '#bf77f6') ;
	at: #limeGreen put: (Color fromString: '#89fe05') ;
	at: #grey put: (Color fromString: '#929591') ;
	at: #skyBlue put: (Color fromString: '#75bbfd') ;
	at: #yellow put: (Color fromString: '#ffff14') ;
	at: #magenta put: (Color fromString: '#c20078') ;
	at: #lightGreen put: (Color fromString: '#96f97b') ;
	at: #orange put: (Color fromString: '#f97306') ;
	at: #teal put: (Color fromString: '#029386') ;
	at: #lightBlue put: (Color fromString: '#95d0fc') ;
	at: #red put: (Color fromString: '#e50000') ;
	at: #brown put: (Color fromString: '#653700') ;
	at: #pink put: (Color fromString: '#ff81c0') ;
	at: #blue put: (Color fromString: '#0343df') ;
	at: #green put: (Color fromString: '#15b01a') ;
	at: #purple put: (Color fromString: '#7e1e9c') ;
  	yourself.

	^xkcdDict

! !

!methodRemoval: Color class #darktan!
Color class removeSelector: #darktan!
!methodRemoval: Color class #fromUser!
Color class removeSelector: #fromUser!
!methodRemoval: Color class #oldColorPaletteForDepth:extent:!
Color class removeSelector: #oldColorPaletteForDepth:extent:!
!methodRemoval: Color class #paleBlue!
Color class removeSelector: #paleBlue!
!methodRemoval: Color class #paleBuff!
Color class removeSelector: #paleBuff!
!methodRemoval: Color class #paleMagenta!
Color class removeSelector: #paleMagenta!
!methodRemoval: Color class #paleOrange!
Color class removeSelector: #paleOrange!
!methodRemoval: Color class #palePeach!
Color class removeSelector: #palePeach!
!methodRemoval: Color class #paleRed!
Color class removeSelector: #paleRed!
!methodRemoval: Color class #paleTan!
Color class removeSelector: #paleTan!
!methodRemoval: Color class #paleYellow!
Color class removeSelector: #paleYellow!
!methodRemoval: Color class #veryPaleRed!
Color class removeSelector: #veryPaleRed!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace'
	classVariableNames: 'CachedColormaps ColorNamesDict GrayToIndexMap IndexedColors MaskingMap PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow RandomStream'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

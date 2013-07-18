'From Cuis 4.1 of 12 December 2012 [latest update: #1592] on 14 February 2013 at 9:16:02 am'!

!Color class methodsFor: 'instance creation' stamp: 'jmv 2/14/2013 09:15'!
fromString: aString
	"For HTML color spec: #FFCCAA. Also support named colors.
	See http://www.w3schools.com/cssref/css_colors_legal.asp"
	"
	Color fromString: '#FFCCAA'.
	Color fromString: 'white'.
	Color fromString: 'orange'
	Color fromString: 'rgb(255,0,98)'
	"
	(aString size = 7 and: [ aString first = $# ]) ifTrue: [
		| aColorHexU red green blue |
		aColorHexU _ aString asUppercase.
		red _ ('16r', (aColorHexU copyFrom: 2 to: 3)) asNumber/255.
		green _ ('16r', (aColorHexU copyFrom: 4 to: 5)) asNumber/255.
		blue _ ('16r', (aColorHexU copyFrom: 6 to: 7)) asNumber/255.
		^ self r: red g: green b: blue].

	(aString beginsWith: 'rgb')
		ifTrue: [|values r g b|
				values := (aString allButFirst: 4) allButLast findTokens: ','.
				r  := (values first includes: $%)
						ifTrue: [(values first asNumber min: 100 max: 0) / 100.0]
						ifFalse: [(values first asInteger min: 255 max: 0) / 255.0].
				g := (values second includes: $%)
						ifTrue: [(values second asNumber min: 100 max: 0) / 100.0]
						ifFalse: [(values second asInteger min: 255 max: 0) / 255.0].
				b := (values third includes: $%)
						ifTrue: [(values third asNumber min: 100 max: 0) / 100.0]
						ifFalse: [(values third asInteger min: 255 max: 0) / 255.0].
				^self r: r g: g b: b].

	^(self exactColorNamed: aString asLowercase)
		ifNil: [ Color white ]! !

!Color class methodsFor: 'class initialization' stamp: 'jmv 2/14/2013 09:02'!
initializeNames
	"Name some colors."

	self setColorNamesDict: (self defaultColorNamesDictionary).
	"Add keyword colors as required"		
	self colorNamesDict keysAndValuesDo: 
		[ :colorName :color | self named: colorName put: color]! !

!methodRemoval: Color class #css2NamedColors!
Color class removeSelector: #css2NamedColors!
!methodRemoval: Color class #fromCSS2String:!
Color class removeSelector: #fromCSS2String:!

!Color class reorganize!
('instance creation' colorFrom: colorFromPixelValue:depth: fromArray: fromRgbTriplet: fromString: gray: h:s:v: h:s:v:alpha: hue:chroma:brightness: hue:chroma:luminance: hue:saturation:brightness: new r:g:b: r:g:b:alpha: r:g:b:range: random random2)
('class initialization' initialize initializeGrayToIndexMap initializeHighLights initializeIndexedColors initializeNames initializeTranslucentPatterns named:put:)
('examples' colorRampForDepth:extent: experimentsTowarsANewColorPalette hotColdShades: showColorCube showColors: showHSVPalettes showHuesInteractively wheel: wheel:saturation:brightness:)
('named colors' aqua beige black blue brightGreen brown cyan darkBlue darkGray darkGreen darkPink darkPurple darktan forestGreen gray green grey hotPink indigo lavender lightBlue lightBrown lightCyan lightGray lightGreen lightMagenta lightOrange lightPink lightPurple lightRed lightYellow lilac lime limeGreen magenta maroon mauve mustard navyBlue olive oliveGreen orange paleBlue paleBuff paleGreen paleMagenta paleOrange palePeach paleRed paleTan paleYellow peach periwinkle pink purple red royalBlue salmon seaGreen skyBlue tan teal transparent turquoise veryDarkGray veryLightGray veryPaleRed veryVeryDarkGray veryVeryLightGray violet white yellow)
('colormaps' cachedColormapFrom:to: colorMapIfNeededFrom:to: computeColorConvertingMap:from:to:keepSubPixelAA: computeColormapFrom:to: computeIndexedColorConvertingMap:from:to: computeRGBColorConvertingMap:to:keepSubPixelAA: computeRGBColormapFor:bitsPerColor:)
('other' indexedColors maskingMap: pixelScreenForDepth: quickHighLight: shutDown translucentMaskFor:depth:)
('color from user' colorPaletteForDepth:extent: colorTest:extent:colorMapper: fromUser oldColorPaletteForDepth:extent:)
('instance protocol testing' gatherProtocols)
('color name lookup' colorNames colorNamesDict defaultColorNamesDictionary doesNotUnderstand: exactColorNamed: setColorNamesDict: traditionalColorNamesDictionary xkcdFirst48ColorNamesDictionary)
('selection' blueColorDict blueColorDict: blueGreenColorDict blueGreenColorDict: brightColorDict brightColorDict: brownColorDict brownColorDict: darkColorDict darkColorDict: grayColorDict: greenColorDict greenColorDict: greyColorDict greyColorDict: lightColorDict lightColorDict: orangeColorDict orangeColorDict: pastelColorDict pastelColorDict: pinkColorDict pinkColorDict: purpleColorDict purpleColorDict: redColorDict redColorDict: saturatedColorDict saturatedColorDict: yellowColorDict yellowColorDict:)
('instance creation - css' css2NamedColors1 css2NamedColors2)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Color initialize!


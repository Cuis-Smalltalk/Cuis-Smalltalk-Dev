'From Cuis 4.1 of 12 December 2012 [latest update: #1583] on 2 February 2013 at 6:20:19 pm'!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern '
	classVariableNames: 'Black Blue BlueShift Brown CSS2NamedColors CachedColormaps ColorChart ColorNames ComponentMask ComponentMax Cyan DarkGray Gray GrayToIndexMap Green GreenShift HalfComponentMask HighLightBitmaps IndexedColors LightBlue LightBrown LightCyan LightGray LightGreen LightMagenta LightOrange LightRed LightYellow Magenta MaskingMap Orange PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow RandomStream Red RedShift TranslucentPatterns Transparent VeryDarkGray VeryLightGray VeryVeryDarkGray VeryVeryLightGray White Yellow OliveGreen Teal Grey BrightGreen LightPink Aqua Olive SeaGreen Lilac HotPink Lime DarkPurple DarkGreen ForestGreen NavyBlue PaleGreen Violet Periwinkle Tan Peach DarkPink SkyBlue Beige Turquoise Purple Salmon DarkBlue LightPurple Lavender LimeGreen Mustard Maroon Indigo RoyalBlue Mauve Pink ColorNamesDict '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color methodsFor: 'equality' stamp: 'KenD 1/24/2013 22:15'!
rgbDistance: otherColor
	"Compare two colors in distance"

	^ ((self privateRed     - otherColor privateRed)    squared)
	+ ((self privateGreen - otherColor privateGreen) squared)
	+ ((self privateBlue    - otherColor privateBlue )   squared)! !

!Color methodsFor: 'named colors' stamp: 'KenD 1/26/2013 16:35'!
closestAssocFrom: aColorDict
	"Answer closest associated color in aColorDict"

"
	((Color lightBlue) closestAssocFrom: (Color namedColors)) explore.
"
	| close distance |
	close := nil.

	distance := 999999999999999999999. "big"
	aColorDict  associationsDo:  [ :assoc || dist |  
		dist := self rgbDistance: (assoc value). 
		(dist < distance) 
		ifTrue: [distance := dist. close := assoc]
	].
	^close! !

!Color methodsFor: 'named colors' stamp: 'KenD 1/26/2013 16:33'!
closestColor
	"Answer the closest matching color in the colorNames dictionary"

"
	(Color red lighter lighter) closestColor explore.
"
	^ self closestColorFrom: (Color colorNamesDict)! !

!Color methodsFor: 'named colors' stamp: 'KenD 1/26/2013 16:36'!
closestColorAssociation
	"Answer this color's closest name->color match."

"
	(Color blue lighter lighter) closestColorAssociation explore.
"	
	^ self closestAssocFrom: (Color colorNamesDict)! !

!Color methodsFor: 'named colors' stamp: 'KenD 1/25/2013 14:31'!
closestColorFrom: aColorDict
	"Answer closest associated color"
"
	((Color lightBlue) closestColorFrom: (Color css2NamedColors)) explore..
"
	^(self closestAssocFrom: aColorDict) value! !

!Color methodsFor: 'named colors' stamp: 'KenD 1/26/2013 16:36'!
closestColorName
	"Answer the name symbol of the closest matching color in the colorNames dictionary"
"
	(Color red lighter lighter) closestColor name.
"
	
	^ self closestNameFrom: (Color colorNamesDict)! !

!Color methodsFor: 'named colors' stamp: 'KenD 1/26/2013 16:36'!
closestNameFrom: aColorDict
	"Answer closest associated color name"
"
	((Color lightBlue) closestNameFrom: (Color namedColors)) print.
"
	^(self closestAssocFrom: aColorDict) key! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:35'!
isBlue
	"Am I considered Blue?"

	^self blue > (self green + 0.3)
		and: [self blue > (self red + 0.3)]
			and: [(self green - self red) abs < 0.4]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:37'!
isBlueGreen
	"Am I considered BlueGreen ?"

	^self green > self red 
		and: [self red < 0.3]
			and: [(self green - self blue) abs < 0.1]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:50'!
isBright
	"Am I considered a Bright color ?"

	^self brightness > 0.6! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:40'!
isBrown
	"Am I considered Brown ?"

	^self red >= self green  
		and: [self green > self blue]					
			and: [(self red - self green) < 0.5]	
				and: [(self green - self blue) < 0.3]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:50'!
isDark
	"Am I considered a Dark color ?"

	^self brightness < 0.5! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:38'!
isGreen
	"Am I considered Green ?"

	^self green > (self blue + 0.3)
		and: [self blue > (self red + 0.3)]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:40'!
isGrey
	"Am I considered Grey  ?"

	^self isGray! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:49'!
isLght
	"Am I considered a Light color ?"

	^self brightness > 0.6! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:44'!
isOrange
	"Am I considered Orange ?"

	^self red > ((self green max: self blue) + 0.2)  
		and: [self green > (self blue + 0.2)]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:45'!
isPastel
	"Am I considered Pastel ?"

	^self saturation < 0.4! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:46'!
isPink
	"Am I considered Pink ?"

	^self red > ((self green max: self blue) + 0.3)  
		and: [self blue > (self green + 0.2)]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:47'!
isRed
	"Am I considered Red ?"

	^self red > (self green + 0.4)
		and: [self red > (self blue + 0.6)]
			and: [(self green - self blue) abs < 0.4]! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:55'!
isSaturated
	"Am I considered to be a Saturated color ?"

	^self saturation > 0.6! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:49'!
isYellow
	"Am I considered Yellow ?"

	^self blue < 0.05 
		and: [(self red min: self green) > 0.5]					
			and: [(self red - self green) abs < 0.2]! !


!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
aqua
	^Aqua! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
beige
	^Beige! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
brightGreen
	^BrightGreen! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
darkBlue
	^DarkBlue! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
darkGreen
	^DarkGreen! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
darkPink
	^DarkPink! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
darkPurple
	^DarkPurple! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
forestGreen
	^ForestGreen! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
grey
	^Grey! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
hotPink
	^HotPink! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
indigo
	^Indigo! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
lavender
	^Lavender! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
lightPink
	^LightPink! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
lightPurple
	^LightPurple! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
lilac
	^Lilac! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
lime
	^Lime! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
limeGreen
	^LimeGreen! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
maroon
	^Maroon! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
mauve
	^Mauve! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
mustard
	^Mustard! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
navyBlue
	^NavyBlue! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
olive
	^Olive! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
oliveGreen
	^OliveGreen! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
peach
	^Peach! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
periwinkle
	^Periwinkle! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
pink
	^Pink! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
purple
	^Purple! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
royalBlue
	^RoyalBlue! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
salmon
	^Salmon! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
seaGreen
	^SeaGreen! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
skyBlue
	^SkyBlue! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
teal
	^Teal! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
turquoise
	^Turquoise! !

!Color class methodsFor: 'named colors' stamp: 'KenD 2/2/2013 18:18'!
violet
	^Violet! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 1/26/2013 20:12'!
colorNamesDict
	"Answer the current dictionary of  name->color associations."
"
	Color colorNamesDict explore.
"
	^ColorNamesDict ! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 1/28/2013 20:34'!
defaultColorNamesDictionary
	"Answer a amall but  useful name->color dictionary"
	
	| defaultDict |
	"Meld most used xkcd colors into traditional colors."
	defaultDict := (self traditionalColorNamesDictionary addAll: 
					self xkcdFirst48ColorNamesDictionary; 
					yourself).
		
	"Override traditional names existing in extended XKCD naming"
	defaultDict at: #lightYellow    put: (Color r: 1.0 g: 0.996 b: 0.478).
	defaultDict at: #lightOrange   put: (Color r: 0.992 g: 0.667 b: 0.283).
	defaultDict at: #lightCyan        put: (Color r: 0.674 g: 1.0 b: 0.988).
	defaultDict at: #lightRed          put: (Color r: 1.0 g: 0.279 b: 0.298).
	defaultDict at: #lightMagenta put: (Color r: 0.98 g: 0.372 b: 0.969).
	
	^defaultDict! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 2/1/2013 14:39'!
doesNotUnderstand: aMessage
	"Some code takes
		 Color colorNames 
	and does
		Color perform: aColorname.
		
	Make this work."

	^(Color colorNamesDict) 
		at: (aMessage selector)
		ifAbsent: [super doesNotUnderstand: aMessage]! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 1/26/2013 20:51'!
exactColorNamed: aColorName

	"Answer color matching name or nil 
	from my ColorName->Color Dictionary"
	
	^ self colorNamesDict at: (aColorName asSymbol) ifAbsent: [nil]! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 1/26/2013 20:15'!
setColorNamesDict: aNameColorDictionary
	"Answer the current dictionary of  name->color associations."
	
	"Note: This is a non-standard name because this is a non-standard action!!
	Do this if you really, really know what you are doing."

	ColorNamesDict := aNameColorDictionary! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 1/28/2013 20:33'!
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
	nameDict at: #transparent put: (TranslucentColor new alpha: 0.0).
	
	^nameDict
! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 1/28/2013 20:33'!
xkcdFirst48ColorNamesDictionary
	"XKCD color names are based on a survey oc colors people call by the same name.
	 http://blog.xkcd.com/2010/05/03/color-survey-results/"

	"Answer a dictionary of the most agreed upon first 48 xkcd colors"
"
	Color xkcdFirst48 explore.
"	
	|xkcdDict|
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

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:26'!
blueColorDict
	"Answer a dictionary of named colors considered Blue"
"
	Color blueColorDict explore.
"
	^self blueColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:24'!
blueColorDict: aColorDict
	"Answer a dictionary of named colors considered Blue"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isBlue)
				or: [assoc key asString asLowercase includesSubString: 'blue'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:28'!
blueGreenColorDict
	"Answer a dictionary of named colors considered Blue"
"
	Color blueGreenColorDict explore.
"
	^self blueGreenColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:29'!
blueGreenColorDict: aColorDict
	"Answer a dictionary of named colors considered Blue"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isBlueGreen)
				or: [assoc key asString asLowercase includesSubString: 'bluegreen'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:31'!
brightColorDict
	"Answer a dictionary of named colors considered Bright"
"
	Color brightColorDict explore.
"
	^self brightColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:31'!
brightColorDict: aColorDict
	"Answer a dictionary of named colors considered Bright"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isBright) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:34'!
brownColorDict
	"Answer a dictionary of named colors considered Brown"
"
	Color brownColorDict explore.
"
	^self brownColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:33'!
brownColorDict: aColorDict
	"Answer a dictionary of named colors considered Brown"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isBrown)
				or: [assoc key asString asLowercase includesSubString: 'brown'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:36'!
darkColorDict
	"Answer a dictionary of named colors considered Dark"
"
	Color darkColorDict explore.
"
	^self darkColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:37'!
darkColorDict: aColorDict
	"Answer a dictionary of named colors considered Dark"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isDark) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:41'!
grayColorDict: aColorDict
	"Answer a dictionary of named colors considered Grey"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isGrey)
				or: [assoc key asString asLowercase includesSubString: 'grey'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:38'!
greenColorDict
	"Answer a dictionary of named colors considered Green"
"
	Color greenColorDict explore.
"
	^self greenColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:38'!
greenColorDict: aColorDict
	"Answer a dictionary of named colors considered Green"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isGreen)
				or: [assoc key asString asLowercase includesSubString: 'green'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:41'!
greyColorDict
	"Answer a dictionary of named colors considered Grey"
"
	Color greyColorDict explore.
"
	^self greyColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:40'!
greyColorDict: aColorDict
	"Answer a dictionary of named colors considered Grey"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isGrey)
				or: [assoc key asString asLowercase includesSubString: 'grey'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:44'!
lightColorDict
	"Answer a dictionary of named colors considered light"
"
	Color lightColorDict explore.
"
	^self lightColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:43'!
lightColorDict: aColorDict
	"Answer a dictionary of named colors considered Liight"

	^self brightColorDict: aColorDict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:46'!
orangeColorDict
	"Answer a dictionary of named colors considered Orange"
"
	Color orangeColorDict explore.
"
	^self orangeColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:46'!
orangeColorDict: aColorDict
	"Answer a dictionary of named colors considered Orange"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isOrange)
				or: [assoc key asString asLowercase includesSubString: 'orange'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:48'!
pastelColorDict
	"Answer a dictionary of named colors considered Pastel"
"
	Color pastelColorDict explore.
"
	^self pastelColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:48'!
pastelColorDict: aColorDict
	"Answer a dictionary of named colors considered Bright"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isPastel) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:50'!
pinkColorDict
	"Answer a dictionary of named colors considered Pink"
"
	Color pinkColorDict explore.
"
	^self pinkColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:50'!
pinkColorDict: aColorDict
	"Answer a dictionary of named colors considered Pink"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isPink)
				or: [assoc key asString asLowercase includesSubString: 'pink'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:52'!
purpleColorDict
	"Answer a dictionary of named colors considered Purple"
"
	Color purpleColorDict explore.
"
	^self purpleColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:52'!
purpleColorDict: aColorDict
	"Answer a dictionary of named colors considered Pink"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc key asString asLowercase includesSubString: 'purple')
				or: [assoc key asString asLowercase includesSubString: 'violet'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:54'!
redColorDict
	"Answer a dictionary of named colors considered Red"
"
	Color redColorDict explore.
"
	^self redColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:53'!
redColorDict: aColorDict
	"Answer a dictionary of named colors considered Red"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isRed)
				or: [assoc key asString asLowercase includesSubString: 'red'])
			ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:56'!
saturatedColorDict
	"Answer a dictionary of named colors considered Saturated"
"
	Color saturatedColorDict explore.
"
	^self saturatedColorDict: (self colorNamesDict )! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:55'!
saturatedColorDict: aColorDict
	"Answer a dictionary of named colors considered Saturated"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isSaturated) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:26'!
yellowColorDict
	"Answer a dictionary of named colors considered Yellow"
"
	Color yellowColorDict explore.
"
	^self yellowColorDict: (self colorNamesDict)! !

!Color class methodsFor: 'selection' stamp: 'KenD 1/30/2013 17:19'!
yellowColorDict: aColorDict
	"Answer a dictionary of named colors considered Yellow"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			((assoc value isYellow)
				or: [assoc key asString asLowercase includesSubString: 'yellow'])
			ifTrue: [dict add: assoc]
		].
	^dict! !


!Color methodsFor: 'other' stamp: 'KenD 1/27/2013 16:40'!
name
	"Return this color's name, or nil if it has no name. Only returns a name if it exactly matches the named color."

	^(Color colorNamesDict keyAtValue: self  ifAbsent: [nil])
! !

!Color methodsFor: 'selection' stamp: 'KenD 1/30/2013 16:42'!
isGray
	"Am I considered Gray  ?"

	^(self red closeTo: self green)
		and: [self blue closeTo: self green ]! !


!Color class methodsFor: 'class initialization' stamp: 'KenD 1/27/2013 15:35'!
initializeNames
	"Name some colors."

	self setColorNamesDict: (self defaultColorNamesDictionary).
	"Add keyword colors as required"		
	self colorNamesDict keysAndValuesDo: 
		[ :name :color | self named: name put: color]! !

!Color class methodsFor: 'class initialization' stamp: 'KenD 1/26/2013 20:35'!
named: newName put: aColor
	"Add an access message and a class variable for aColor.  The newName should start with a lowercase letter.  (The class variable will start with an uppercase letter.)  (Color colorNames) returns a list of all color names.  "
	| str cap sym accessor csym |
	(aColor isKindOf: self) ifFalse: [^ self error: 'not a Color'].
	str _ newName asString.
	sym _ str asSymbol.
	cap _ str capitalized.
	csym _ cap asSymbol.
	(self class canUnderstand: sym) ifFalse: [
		"define access message"
		accessor _ str, String newLineString, String tab, '^', cap.
		self class compile: accessor
			classified: 'named colors'].
	(self classPool includesKey: csym) ifFalse: [
		self addClassVarName: cap].
	^ self classPool at: csym put: aColor! !

!Color class methodsFor: 'color name lookup' stamp: 'KenD 1/26/2013 20:13'!
colorNames
	"Answer the current dictionary of  name->color associations."

	^self colorNamesDict keys! !


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
('*xkcd')
('*css3')
('*nbsiscc')
('selection' blueColorDict blueColorDict: blueGreenColorDict blueGreenColorDict: brightColorDict brightColorDict: brownColorDict brownColorDict: darkColorDict darkColorDict: grayColorDict: greenColorDict greenColorDict: greyColorDict greyColorDict: lightColorDict lightColorDict: orangeColorDict orangeColorDict: pastelColorDict pastelColorDict: pinkColorDict pinkColorDict: purpleColorDict purpleColorDict: redColorDict redColorDict: saturatedColorDict saturatedColorDict: yellowColorDict yellowColorDict:)
('instance creation - css' css2NamedColors css2NamedColors1 css2NamedColors2 fromCSS2String:)
!

!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern'
	classVariableNames: 'Aqua Beige Black Blue BlueShift BrightGreen Brown CachedColormaps ColorChart ColorNamesDict ComponentMask ComponentMax Cyan DarkBlue DarkGray DarkGreen DarkPink DarkPurple ForestGreen Gray GrayToIndexMap Green GreenShift Grey HalfComponentMask HighLightBitmaps HotPink IndexedColors Indigo Lavender LightBlue LightBrown LightCyan LightGray LightGreen LightMagenta LightOrange LightPink LightPurple LightRed LightYellow Lilac Lime LimeGreen Magenta Maroon MaskingMap Mauve Mustard NavyBlue Olive OliveGreen Orange PaleGreen Peach Periwinkle Pink PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow Purple RandomStream Red RedShift RoyalBlue Salmon SeaGreen SkyBlue Tan Teal TranslucentPatterns Transparent Turquoise VeryDarkGray VeryLightGray VeryVeryDarkGray VeryVeryLightGray Violet White Yellow'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color reorganize!
('access' alpha blue brightness chroma green hue icon iconOrThumbnailOfSize: luminance red saturation swatch)
('equality' = diff: hash rgbDistance:)
('queries' isBlack isOpaque isTransparent)
('transformations' * + - / adjustBrightness: adjustSaturation:brightness: alpha: alphaMixed:with: atLeastAsLuminentAs: atMostAsLuminentAs: blacker dansDarker darker duller lighter mixed:with: muchDarker muchLighter negated orColorUnlike: paler quiteBlacker quiteWhiter slightlyDarker slightlyLighter slightlyWhiter twiceDarker twiceLighter veryMuchDarker veryMuchLighter whiter)
('groups of shades' darkShades: lightShades: mix:shades: wheel:)
('printing' printOn: shortPrintOn: shortPrintString storeArrayOn: storeArrayValuesOn: storeOn:)
('other' display name rgbTriplet)
('conversions' asColor asColorref asNontranslucentColor balancedPatternForDepth: bitPatternForDepth: closestPixelValue1 closestPixelValue2 closestPixelValue4 closestPixelValue8 dominantColor halfTonePattern1 halfTonePattern2 indexInMap: makeForegroundColor pixelValue32 pixelValueForDepth: pixelWordFor:filledWith: pixelWordForDepth: scaledPixelValue32)
('private' attemptToMutateError flushCache privateAlpha privateBlue privateGreen privateRGB privateRed setHue:chroma:brightness: setHue:chroma:luminance: setHue:saturation:brightness: setPrivateRed:green:blue: setRGB: setRed:green:blue: setRed:green:blue:range:)
('testing' mightBeTranslucent)
('objects from disk' objectForDataStream:)
('named colors' closestAssocFrom: closestColor closestColorAssociation closestColorFrom: closestColorName closestNameFrom:)
('selection' isBlue isBlueGreen isBright isBrown isDark isGray isGreen isGrey isLght isOrange isPastel isPink isRed isSaturated isYellow)
!


'From Cuis 4.2 of 25 July 2013 [latest update: #2023] on 30 June 2014 at 3:13:43.921116 pm'!
!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace '
	classVariableNames: 'Aqua Beige Black Blue BrightGreen Brown CachedColormaps ColorChart ColorNamesDict Cyan DarkBlue DarkGray DarkGreen DarkPink DarkPurple ForestGreen Gray GrayToIndexMap Green Grey HotPink IndexedColors Indigo Lavender LightBlue LightBrown LightCyan LightGray LightGreen LightMagenta LightOrange LightPink LightPurple LightRed LightYellow Lilac Lime LimeGreen Magenta Maroon MaskingMap Mauve Mustard NavyBlue Olive OliveGreen Orange PaleGreen Peach Periwinkle Pink PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow Purple RandomStream Red RoyalBlue Salmon SeaGreen SkyBlue Tan Teal Transparent Turquoise VeryDarkGray VeryLightGray VeryVeryDarkGray VeryVeryLightGray Violet White Yellow '
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:55'!
aqua

	^ ColorNamesDict at: #aqua! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:56'!
beige

	^ ColorNamesDict at: #beige! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:56'!
black

	^ ColorNamesDict at: #black! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:56'!
blue

	^ ColorNamesDict at: #blue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:57'!
brightGreen

	^ ColorNamesDict at: #brightGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:57'!
brown

	^ ColorNamesDict at: #brown! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:57'!
cyan

	^ ColorNamesDict at: #cyan! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:57'!
darkBlue

	^ ColorNamesDict at: #darkBlue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:57'!
darkGray

	^ ColorNamesDict at: #darkGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:57'!
darkGreen

	^ ColorNamesDict at: #darkGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:57'!
darkPink

	^ ColorNamesDict at: #darkPink! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:57'!
darkPurple

	^ ColorNamesDict at: #darkPurple! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:57'!
forestGreen

	^ ColorNamesDict at: #forestGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:11'!
gray

	^ ColorNamesDict at: #gray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:58'!
green

	^ ColorNamesDict at: #green! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:58'!
grey

	^ ColorNamesDict at: #grey! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:58'!
hotPink

	^ ColorNamesDict at: #hotPink! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:58'!
indigo

	^ ColorNamesDict at: #indigo! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:58'!
lavender

	^ ColorNamesDict at: #lavender! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:58'!
lightBlue

	^ ColorNamesDict at: #lightBlue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:58'!
lightBrown

	^ ColorNamesDict at: #lightBrown! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:58'!
lightCyan

	^ ColorNamesDict at: #lightCyan! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:58'!
lightGray

	^ ColorNamesDict at: #lightGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:59'!
lightGreen

	^ ColorNamesDict at: #lightGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:59'!
lightMagenta

	^ ColorNamesDict at: #lightMagenta! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:59'!
lightOrange

	^ ColorNamesDict at: #lightOrange! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:59'!
lightPink

	^ ColorNamesDict at: #lightPink! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:59'!
lightPurple

	^ ColorNamesDict at: #lightPurple! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:59'!
lightRed

	^ ColorNamesDict at: #lightRed! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:59'!
lightYellow

	^ ColorNamesDict at: #lightYellow! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:59'!
lilac

	^ ColorNamesDict at: #lilac! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:59'!
lime

	^ ColorNamesDict at: #lime! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 14:59'!
limeGreen

	^ ColorNamesDict at: #limeGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:00'!
magenta

	^ ColorNamesDict at: #magenta! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:00'!
maroon

	^ ColorNamesDict at: #maroon! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:00'!
mauve

	^ ColorNamesDict at: #mauve! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:00'!
mustard

	^ ColorNamesDict at: #mustard! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:00'!
navyBlue

	^ ColorNamesDict at: #navyBlue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:00'!
olive

	^ ColorNamesDict at: #olive! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:00'!
oliveGreen

	^ ColorNamesDict at: #oliveGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:00'!
orange

	^ ColorNamesDict at: #orange! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:00'!
peach

	^ ColorNamesDict at: #peach! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:00'!
periwinkle

	^ ColorNamesDict at: #periwinkle! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:00'!
pink

	^ ColorNamesDict at: #pink! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:12'!
purple

	^ ColorNamesDict at: #purple! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:01'!
red

	^ ColorNamesDict at: #red! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:01'!
royalBlue

	^ ColorNamesDict at: #royalBlue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:01'!
salmon

	^ ColorNamesDict at: #salmon! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:01'!
seaGreen

	^ ColorNamesDict at: #seaGreen! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:01'!
skyBlue

	^ ColorNamesDict at: #skyBlue! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:01'!
teal

	^ ColorNamesDict at: #teal! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:02'!
transparent

	^ ColorNamesDict at: #transparent! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:02'!
turquoise

	^ ColorNamesDict at: #turquoise! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:02'!
veryDarkGray

	^ ColorNamesDict at: #veryDarkGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:02'!
veryLightGray

	^ ColorNamesDict at: #veryLightGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:02'!
veryVeryDarkGray

	^ ColorNamesDict at: #veryVeryDarkGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:02'!
veryVeryLightGray

	^ ColorNamesDict at: #veryVeryLightGray! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:03'!
violet

	^ ColorNamesDict at: #violet! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:03'!
white

	^ ColorNamesDict at: #white! !

!Color class methodsFor: 'named colors' stamp: 'jmv 6/30/2014 15:03'!
yellow

	^ ColorNamesDict at: #yellow! !

!classDefinition: #Color category: #'Graphics-Primitives'!
Object subclass: #Color
	instanceVariableNames: 'rgb cachedDepth cachedBitPattern floatRGB colorSpace'
	classVariableNames: 'CachedColormaps ColorChart ColorNamesDict GrayToIndexMap IndexedColors MaskingMap PureBlue PureCyan PureGreen PureMagenta PureRed PureYellow RandomStream'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

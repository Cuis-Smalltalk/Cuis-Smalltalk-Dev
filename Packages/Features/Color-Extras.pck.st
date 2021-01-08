'From Cuis 5.0 [latest update: #4520] on 8 January 2021 at 3:11:05 pm'!
'Description '!
!provides: 'Color-Extras' 1 8!
SystemOrganization addCategory: 'Color-Extras'!


!classDefinition: #ColorExtrasPackage category: 'Color-Extras'!
CodePackage subclass: #ColorExtrasPackage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Color-Extras'!
!classDefinition: 'ColorExtrasPackage class' category: 'Color-Extras'!
ColorExtrasPackage class
	instanceVariableNames: ''!


!ColorExtrasPackage class methodsFor: 'installing' stamp: 'jmv 6/1/2018 20:58:19'!
postPackageInstall
	"This gets called after installing all the package code"

	Color initializeColorExtras! !

!ColorExtrasPackage class methodsFor: 'installing' stamp: 'jmv 6/1/2018 20:57:48'!
prePackageInstall
	"This gets called after creating the package class and installing its code, but before installing the rest of the package code"

	Color classPool declare: #ColorNamesDict from: Undeclared! !
ColorExtrasPackage prePackageInstall!

!Color methodsFor: '*color-extras-named colors' stamp: 'KenD 12/8/2013 14:59'!
closestAssocFrom: aColorDict
	"Answer closest associated color in aColorDict"
"
	((Color r: 0.3 g: 0.2 b: 0.8) closestAssocFrom: (Color colorNamesDict)) explore.
"
	"Filter values for those close in hue (cheap test) then use rgbDistance"
	| closeInHue isClose close distance myHue |
	closeInHue := OrderedCollection new.
	myHue := self hue.
	isClose := [ :assoc | | delta |
		delta := ((assoc value hue) - myHue) abs.
		(delta < 20) or: [ delta > 340 ] "hues within 20 degrees"
	].
	aColorDict  associationsDo:  [ :assoc |
		(isClose value: assoc) ifTrue: [ closeInHue add: assoc ]
	].

	close := nil.
	distance := 1000. "big"
	(closeInHue size > 0)
		ifFalse: [ "fallback -- no color really close"
			aColorDict  associationsDo:  [ :assoc | | dist |
				dist := self rgbDistance: (assoc value). 
				(dist < distance)  ifTrue: [distance := dist. close := assoc]
				]
		]
		ifTrue: [ closeInHue do: [ :assoc | | dist |
				dist := self rgbDistance: (assoc value). 
				(dist < distance)  ifTrue: [distance := dist. close := assoc]
				]
		].
	
	^ close! !

!Color methodsFor: '*color-extras-named colors' stamp: 'KenD 1/26/2013 16:33'!
closestColor
	"Answer the closest matching color in the colorNames dictionary"

"
	(Color red lighter lighter) closestColor explore.
"
	^ self closestColorFrom: (Color colorNamesDict)! !

!Color methodsFor: '*color-extras-named colors' stamp: 'KenD 1/26/2013 16:36'!
closestColorAssociation
	"Answer this color's closest name->color match."

"
	(Color blue lighter lighter) closestColorAssociation explore.
"	
	^ self closestAssocFrom: (Color colorNamesDict)! !

!Color methodsFor: '*color-extras-named colors' stamp: 'KenD 1/25/2013 14:31'!
closestColorFrom: aColorDict
	"Answer closest associated color"
"
	((Color lightBlue) closestColorFrom: (Color css2NamedColors)) explore..
"
	^(self closestAssocFrom: aColorDict) value! !

!Color methodsFor: '*color-extras-named colors' stamp: 'KenD 1/26/2013 16:36'!
closestColorName
	"Answer the name symbol of the closest matching color in the colorNames dictionary"
"
	(Color red lighter lighter) closestColor name.
"
	
	^ self closestNameFrom: (Color colorNamesDict)! !

!Color methodsFor: '*color-extras-named colors' stamp: 'KenD 1/26/2013 16:36'!
closestNameFrom: aColorDict
	"Answer closest associated color name"
"
	((Color lightBlue) closestNameFrom: (Color namedColors)) print.
"
	^(self closestAssocFrom: aColorDict) key! !

!Color methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 16:37'!
isBlueGreen
	"Am I considered BlueGreen ?"

	^self green > self red 
		and: [self red < 0.3]
			and: [(self green - self blue) abs < 0.1]! !

!Color methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 16:50'!
isBright
	"Am I considered a Bright color ?"

	^self brightness > 0.6! !

!Color methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 16:40'!
isBrown
	"Am I considered Brown ?"

	^self red >= self green  
		and: [self green > self blue]					
			and: [(self red - self green) < 0.5]	
				and: [(self green - self blue) < 0.3]! !

!Color methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 16:50'!
isDark
	"Am I considered a Dark color ?"

	^self brightness < 0.5! !

!Color methodsFor: '*color-extras-selection' stamp: 'jmv 12/17/2018 10:45:56'!
isGray
	"Am I considered Gray  ?"

	^ self saturation < 0.03! !

!Color methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:40'!
isGrey
	"Am I considered Grey  ?"

	^self isGray! !

!Color methodsFor: '*color-extras-selection' stamp: 'jmv 4/19/2013 17:06'!
isLight
	"Am I considered a Light color ?"

	^self brightness > 0.6! !

!Color methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 16:44'!
isOrange
	"Am I considered Orange ?"

	^self red > ((self green max: self blue) + 0.2)  
		and: [self green > (self blue + 0.2)]! !

!Color methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 16:45'!
isPastel
	"Am I considered Pastel ?"

	^self saturation < 0.4! !

!Color methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 16:46'!
isPink
	"Am I considered Pink ?"

	^self red > ((self green max: self blue) + 0.3)  
		and: [self blue > (self green + 0.2)]! !

!Color methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 16:55'!
isSaturated
	"Am I considered to be a Saturated color ?"

	^self saturation > 0.6! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:58'!
aqua

	^ ColorNamesDict at: #aqua! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:58'!
beige

	^ ColorNamesDict at: #beige! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:26'!
blueColorDict
	"Answer a dictionary of named colors considered Blue"
"
	Color blueColorDict explore.
"
	^self blueColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:24'!
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

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:28'!
blueGreenColorDict
	"Answer a dictionary of named colors considered Blue"
"
	Color blueGreenColorDict explore.
"
	^self blueGreenColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:29'!
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

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:31'!
brightColorDict
	"Answer a dictionary of named colors considered Bright"
"
	Color brightColorDict explore.
"
	^self brightColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:31'!
brightColorDict: aColorDict
	"Answer a dictionary of named colors considered Bright"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isBright) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:58'!
brightGreen

	^ ColorNamesDict at: #brightGreen! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:34'!
brownColorDict
	"Answer a dictionary of named colors considered Brown"
"
	Color brownColorDict explore.
"
	^self brownColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:33'!
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

!Color class methodsFor: '*color-extras-color name lookup' stamp: 'KenD 1/26/2013 20:13'!
colorNames
	"Answer the current dictionary of  name->color associations."

	^ColorNamesDict keys! !

!Color class methodsFor: '*color-extras-cached state access' stamp: 'jmv 3/27/2018 10:51:21'!
colorNamesDict
	"Answer the current dictionary of  name->color associations."
"
	Color colorNamesDict explore.
"
	^ColorNamesDict! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:59'!
darkBlue

	^ ColorNamesDict at: #darkBlue! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:36'!
darkColorDict
	"Answer a dictionary of named colors considered Dark"
"
	Color darkColorDict explore.
"
	^self darkColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:37'!
darkColorDict: aColorDict
	"Answer a dictionary of named colors considered Dark"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isDark) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:59'!
darkGreen

	^ ColorNamesDict at: #darkGreen! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:59'!
darkPink

	^ ColorNamesDict at: #darkPink! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:59'!
darkPurple

	^ ColorNamesDict at: #darkPurple! !

!Color class methodsFor: '*color-extras-color name lookup' stamp: 'KenD 1/28/2013 20:34'!
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

!Color class methodsFor: '*color-extras-color name lookup' stamp: 'KenD 2/1/2013 14:39'!
doesNotUnderstand: aMessage
	"Some code takes
		 Color colorNames 
	and does
		Color perform: aColorname.
		
	Make this work."

	^(Color colorNamesDict) 
		at: (aMessage selector)
		ifAbsent: [super doesNotUnderstand: aMessage]! !

!Color class methodsFor: '*color-extras-color name lookup' stamp: 'KenD 1/26/2013 20:51'!
exactColorNamed: aColorName

	"Answer color matching name or nil 
	from my ColorName->Color Dictionary"
	
	^ ColorNamesDict at: (aColorName asSymbol) ifAbsent: [nil]! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:59'!
forestGreen

	^ ColorNamesDict at: #forestGreen! !

!Color class methodsFor: '*color-extras-instance creation' stamp: 'jmv 1/8/2021 13:07:21'!
fromString: aString
	"For HTML color spec: #FFCCAA. Also support named colors.
	See http://www.w3schools.com/cssref/css_colors_legal.asp"
	"
	Color fromString: '#FFCCAA'.
	Color fromString: 'white'.
	Color fromString: 'orange'
	Color fromString: 'rgb(255,0,98)'
	"
	(self fromHexString: aString) ifNotNil: [ :color |
		^ color ].

	(aString beginsWith: 'rgb')
		ifTrue: [|values r g b|
				values := (aString allButFirst: 4) allButLast findTokens: ','.
				r  := (values first includes: $%)
						ifTrue: [(values first asNumber min: 100 max: 0) / 100.0]
						ifFalse: [(values first asNumber min: 255 max: 0) / 255.0].
				g := (values second includes: $%)
						ifTrue: [(values second asNumber min: 100 max: 0) / 100.0]
						ifFalse: [(values second asNumber min: 255 max: 0) / 255.0].
				b := (values third includes: $%)
						ifTrue: [(values third asNumber min: 100 max: 0) / 100.0]
						ifFalse: [(values third asNumber min: 255 max: 0) / 255.0].
				^self r: r g: g b: b].

	^self exactColorNamed: aString! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:41'!
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

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:38'!
greenColorDict
	"Answer a dictionary of named colors considered Green"
"
	Color greenColorDict explore.
"
	^self greenColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:38'!
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

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:59'!
grey

	^ ColorNamesDict at: #grey! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:41'!
greyColorDict
	"Answer a dictionary of named colors considered Grey"
"
	Color greyColorDict explore.
"
	^self greyColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:40'!
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

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:59'!
hotPink

	^ ColorNamesDict at: #hotPink! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:59'!
indigo

	^ ColorNamesDict at: #indigo! !

!Color class methodsFor: '*color-extras-initialization' stamp: 'jmv 3/27/2018 10:52:59'!
initializeColorExtras

	ColorNamesDict ifNil: [
		self setColorNamesDict: self defaultColorNamesDictionary ]! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 14:59'!
lavender

	^ ColorNamesDict at: #lavender! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:44'!
lightColorDict
	"Answer a dictionary of named colors considered light"
"
	Color lightColorDict explore.
"
	^self lightColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:43'!
lightColorDict: aColorDict
	"Answer a dictionary of named colors considered Liight"

	^self brightColorDict: aColorDict! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
lightPink

	^ ColorNamesDict at: #lightPink! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
lightPurple

	^ ColorNamesDict at: #lightPurple! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
lilac

	^ ColorNamesDict at: #lilac! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
lime

	^ ColorNamesDict at: #lime! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
limeGreen

	^ ColorNamesDict at: #limeGreen! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
maroon

	^ ColorNamesDict at: #maroon! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
mauve

	^ ColorNamesDict at: #mauve! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
mustard

	^ ColorNamesDict at: #mustard! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
navyBlue

	^ ColorNamesDict at: #navyBlue! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
olive

	^ ColorNamesDict at: #olive! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
oliveGreen

	^ ColorNamesDict at: #oliveGreen! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:46'!
orangeColorDict
	"Answer a dictionary of named colors considered Orange"
"
	Color orangeColorDict explore.
"
	^self orangeColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:46'!
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

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
paleGreen

	^ ColorNamesDict at: #paleGreen! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:48'!
pastelColorDict
	"Answer a dictionary of named colors considered Pastel"
"
	Color pastelColorDict explore.
"
	^self pastelColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:48'!
pastelColorDict: aColorDict
	"Answer a dictionary of named colors considered Bright"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isPastel) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
peach

	^ ColorNamesDict at: #peach! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:00'!
periwinkle

	^ ColorNamesDict at: #periwinkle! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:50'!
pinkColorDict
	"Answer a dictionary of named colors considered Pink"
"
	Color pinkColorDict explore.
"
	^self pinkColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:50'!
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

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:52'!
purpleColorDict
	"Answer a dictionary of named colors considered Purple"
"
	Color purpleColorDict explore.
"
	^self purpleColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:52'!
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

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:54'!
redColorDict
	"Answer a dictionary of named colors considered Red"
"
	Color redColorDict explore.
"
	^self redColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:53'!
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

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:01'!
royalBlue

	^ ColorNamesDict at: #royalBlue! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:01'!
salmon

	^ ColorNamesDict at: #salmon! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:56'!
saturatedColorDict
	"Answer a dictionary of named colors considered Saturated"
"
	Color saturatedColorDict explore.
"
	^self saturatedColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:55'!
saturatedColorDict: aColorDict
	"Answer a dictionary of named colors considered Saturated"

	| dict |
	dict := Dictionary new.
	aColorDict associationsDo: 
		 [ :assoc |
			(assoc value isSaturated) ifTrue: [dict add: assoc]
		].
	^dict! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:01'!
seaGreen

	^ ColorNamesDict at: #seaGreen! !

!Color class methodsFor: '*color-extras-color name lookup' stamp: 'KenD 7/25/2014 21:22'!
setColorNamesDict: aNameColorDictionary
	"Answer the current dictionary of  name->color associations."
	
	"Note: This is a non-standard name because this is a non-standard action!!
	Do this if you really, really know what you are doing."
	
	"Some Colors are required for proper system operation"
	Color defaultColorNamesDictionary keysAndValuesDo: [ :colorName :colorValue |
		aNameColorDictionary at: colorName ifAbsentPut: colorValue
	].

	ColorNamesDict := aNameColorDictionary! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:01'!
skyBlue

	^ ColorNamesDict at: #skyBlue! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:01'!
teal

	^ ColorNamesDict at: #teal! !

!Color class methodsFor: '*color-extras-color name lookup' stamp: 'jmv 3/10/2018 20:42:11'!
traditionalColorNamesDictionary
	"Answer a dictionary of Squeak traditional name->color associations.."

	| nameDict |
	nameDict _ Dictionary new.
	nameDict at: #black put: `Color r: 0 g: 0 b: 0`.
	nameDict at: #veryVeryDarkGray put: `Color r: 0.125 g: 0.125 b: 0.125`.
	nameDict at: #veryDarkGray put: `Color r: 0.25 g: 0.25 b: 0.25`.
	nameDict at: #darkGray put: `Color r: 0.375 g: 0.375 b: 0.375`.
	nameDict at: #gray put: `Color r: 0.5 g: 0.5 b: 0.5`.
	nameDict at: #lightGray put: `Color r: 0.625 g: 0.625 b: 0.625`.
	nameDict at: #veryLightGray put: `Color r: 0.75 g: 0.75 b: 0.75`.
	nameDict at: #veryVeryLightGray put: `Color r: 0.875 g: 0.875 b: 0.875`.
	nameDict at: #white put: `Color r: 1.0 g: 1.0 b: 1.0`.
	nameDict at: #red put: `Color r: 1.0 g: 0 b: 0`.
	nameDict at: #yellow put: `Color r: 1.0 g: 1.0 b: 0`.
	nameDict at: #green put: `Color r: 0 g: 1.0 b: 0`.
	nameDict at: #cyan put: `Color r: 0 g: 1.0 b: 1.0`.
	nameDict at: #blue put: `Color r: 0 g: 0 b: 1.0`.
	nameDict at: #magenta put: `Color r: 1.0 g: 0 b: 1.0`.
	nameDict at: #brown put: `Color r: 0.6 g: 0.2 b: 0`.
	nameDict at: #orange put: `Color r: 1.0 g: 0.6 b: 0`.
	nameDict at: #lightRed put: `Color r: 1.0 g: 0.8 b: 0.8`.
	nameDict at: #lightYellow put: `Color r: 1.0 g: 1.0 b: 0.8`.
	nameDict at: #lightGreen put: `Color r: 0.8 g: 1.0 b: 0.6`.
	nameDict at: #lightCyan put: `Color r: 0.4 g: 1.0 b: 1.0`.
	nameDict at: #lightBlue put: `Color r: 0.8 g: 1.0 b: 1.0`.
	nameDict at: #lightMagenta put: `Color r: 1.0 g: 0.8 b: 1.0`.
	nameDict at: #lightBrown put: `Color r: 1.0 g: 0.6 b: 0.2`.
	nameDict at: #lightOrange put: `Color r: 1.0 g: 0.8 b: 0.4`.
	nameDict at: #transparent put: `TranslucentColor r: 0.0 g: 0.0 b: 0.0 alpha: 0.0`.
	
	^nameDict
! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:01'!
turquoise

	^ ColorNamesDict at: #turquoise! !

!Color class methodsFor: '*color-extras-named colors' stamp: 'jmv 4/17/2015 15:01'!
violet

	^ ColorNamesDict at: #violet! !

!Color class methodsFor: '*color-extras-color name lookup' stamp: 'jmv 6/30/2014 15:22'!
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

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:26'!
yellowColorDict
	"Answer a dictionary of named colors considered Yellow"
"
	Color yellowColorDict explore.
"
	^self yellowColorDict: ColorNamesDict! !

!Color class methodsFor: '*color-extras-selection' stamp: 'KenD 1/30/2013 17:19'!
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
ColorExtrasPackage postPackageInstall!

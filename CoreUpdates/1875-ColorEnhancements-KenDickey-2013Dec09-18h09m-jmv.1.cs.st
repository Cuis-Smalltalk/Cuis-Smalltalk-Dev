'From Cuis 4.2 of 25 July 2013 [latest update: #1874] on 9 December 2013 at 6:10:30.260187 pm'!

!Color methodsFor: 'equality' stamp: 'KenD 12/8/2013 08:35'!
rgbDistance: otherColor
	"Compare two colors in distance"
"
	^ (self hue - otherColor hue) abs +
	  (self saturation - otherColor saturation) abs *10 +
	  (self brightness - otherColor brightness) abs
"
	"See http://www.compuphase.com/cmetric.htm"
	| meanRed deltaRed deltaGreen deltaBlue |
	meanRed := (self red + otherColor red) abs / 2.
	deltaRed := (self red - otherColor red) abs.
	deltaGreen := (self green - otherColor green) abs.
	deltaBlue := (self blue - otherColor blue) abs.
	
	^ ( ((2 + (meanRed / 256)) * (deltaRed * deltaRed)) +
		(4 * deltaGreen) +
		((2 + ((255 - meanRed) / 256)) * deltaBlue)
	  ) sqrt! !

!Color methodsFor: 'named colors' stamp: 'KenD 12/8/2013 14:59'!
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


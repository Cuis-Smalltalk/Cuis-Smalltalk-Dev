"
A more sophisticated morph, showing stepping and submorphs. See how little code is needed for a good deal of functionality!

See also class comment at Sample01Star.

Evaluate:
Sample09Clock new openInWorld
"
Class {
	#name : 'Sample09Clock',
	#superclass : 'PlacedMorph',
	#instVars : [
		'hourHand',
		'minuteHand',
		'secondHand'
	],
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'drawing' }
Sample09Clock >> drawOn: aCanvas [

	| f |
	aCanvas
		strokeWidth: 10 color: `Color lightCyan`
		fillColor: `Color veryVeryLightGray lighter` do: [ :canvas |
			canvas circleCenter:  0@0 radius: 100 ].
	f := FontFamily familyName: 'DejaVu Sans' pointSize: 14.
	aCanvas drawString: 'XII' at: `-13 @ 58` font: f color: `Color brown`.
	aCanvas drawString: 'III' at: `73 @ -24` font: f color: `Color brown`.
	aCanvas drawString: 'VI' at: `-9 @ -106` font: f color: `Color brown`.
	aCanvas drawString: 'IX' at: `-90 @ -24` font: f color: `Color brown`.

	f := FontFamily familyName: 'Alex Brush' pointSize: 16.
	aCanvas drawString: 'Chaque Heure' atCenterX: `0@ 18` font: f color: `Color black`.
	f := FontFamily familyName: 'Alex Brush' pointSize: 8.
	aCanvas drawString: 'pour la Minorie' atCenterX: `0@ -60` font: f color: `Color black`.
]

{ #category : 'initialization' }
Sample09Clock >> initialize [

	super initialize.
	self addMorph: (hourHand := Sample09ClockHourHand new).
	self addMorph: (minuteHand := Sample09ClockMinuteHand new).
	self addMorph: (secondHand := Sample09ClockSecondHand new).

	(FontFamily familyName: 'Alex Brush' pointSize: 16) ifNil: [
		TrueTypeFontFamily readTrueTypeFontEntry:
			DirectoryEntry trueTypeFontsDirectory / 'AlexBrush' // 'AlexBrush-Regular.ttf' ]
]

{ #category : 'stepping' }
Sample09Clock >> step [

	| time |
	time := Time now.
	hourHand rotationDegrees: time hour * -30 + (time minute * -0.5).
	minuteHand rotationDegrees: time minute * -6.
	secondHand rotationDegrees: time second * -6.
]

{ #category : 'stepping' }
Sample09Clock >> wantsSteps [

	^ true
]

{ #category : 'geometry testing' }
Sample09Clock >> yAxisPointsUp [
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^true
]

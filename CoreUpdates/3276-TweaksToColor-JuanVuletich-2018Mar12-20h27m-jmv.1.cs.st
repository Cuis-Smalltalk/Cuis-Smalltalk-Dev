'From Cuis 5.0 of 7 November 2016 [latest update: #3275] on 12 March 2018 at 8:29:55 pm'!

!Color class methodsFor: 'instance creation' stamp: 'jmv 3/12/2018 19:29:16'!
fromHexString: aString
	"For HTML color spec: #FFCCAA.
	See http://www.w3schools.com/cssref/css_colors_legal.asp"
	"
	Color fromHexString: '#FFCCAA'.
	"
	(aString size = 7 and: [ aString first = $# ]) ifTrue: [
		| aColorHexU red green blue |
		aColorHexU _ aString asUppercase.
		red _ ('16r', (aColorHexU copyFrom: 2 to: 3)) asNumber/255.
		green _ ('16r', (aColorHexU copyFrom: 4 to: 5)) asNumber/255.
		blue _ ('16r', (aColorHexU copyFrom: 6 to: 7)) asNumber/255.
		^ self r: red g: green b: blue].
	^ nil! !

!Color class methodsFor: 'instance creation' stamp: 'jmv 3/12/2018 19:29:47'!
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
						ifFalse: [(values first asInteger min: 255 max: 0) / 255.0].
				g := (values second includes: $%)
						ifTrue: [(values second asNumber min: 100 max: 0) / 100.0]
						ifFalse: [(values second asInteger min: 255 max: 0) / 255.0].
				b := (values third includes: $%)
						ifTrue: [(values third asNumber min: 100 max: 0) / 100.0]
						ifFalse: [(values third asInteger min: 255 max: 0) / 255.0].
				^self r: r g: g b: b].

	^self exactColorNamed: aString! !


!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 20:28:53'!
black
	"Override traditional names existing in XKCD naming"
	^`Color fromHexString: '#000000'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:30:14'!
blue
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#0343df'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:31:13'!
brown
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#653700'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 20:28:59'!
cyan
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#00ffff'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:33:50'!
green
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#15b01a'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 19:34:21'!
lightBlue
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#95d0fc'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 20:12:45'!
lightBrown
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#ad8150'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 20:13:44'!
lightGreen
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#96f97b'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 20:14:56'!
magenta
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#c20078'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 20:15:20'!
orange
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#f97306'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 20:16:35'!
pink
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#ff81c0'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 20:16:53'!
purple
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#7e1e9c'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 20:17:13'!
red
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#e50000'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 20:28:47'!
tan
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#d1b26f'`! !

!Color class methodsFor: 'named colors' stamp: 'jmv 3/12/2018 20:18:33'!
yellow
	"Override traditional names existing in XKCD naming"
	^ `Color fromHexString: '#ffff14'`! !


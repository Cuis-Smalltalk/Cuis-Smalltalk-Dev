'From Cuis 4.1 of 12 December 2012 [latest update: #1593] on 14 February 2013 at 9:43:28 am'!

!Color class methodsFor: 'instance creation' stamp: 'jmv 2/14/2013 09:23'!
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

	^self exactColorNamed: aString asLowercase! !


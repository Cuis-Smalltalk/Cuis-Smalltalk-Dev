'From Cuis 5.0 [latest update: #4330] on 3 August 2020 at 5:30:57 pm'!

!Color class methodsFor: 'instance creation' stamp: 'jmv 8/3/2020 17:30:42'!
fromHexString: aString
	"For HTML color spec: #FFCCAA.
	See http://www.w3schools.com/cssref/css_colors_legal.asp
	Also handles 3 digit shorthand."
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
	(aString size = 4 and: [ aString first = $# ]) ifTrue: [
		| aColorHexU red green blue |
		aColorHexU _ aString asUppercase.
		red _ aColorHexU at: 2.
		green _ aColorHexU at: 3.
		blue _ aColorHexU at: 4.
		red _ ('16r', (String with: red with: red)) asNumber/255.
		green _ ('16r', (String with: green with: green)) asNumber/255.
		blue _ ('16r', (String with: blue with: blue)) asNumber/255.
		^ self r: red g: green b: blue].
	^ nil! !


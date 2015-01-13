'From Cuis 4.2 of 25 July 2013 [latest update: #2152] on 9 January 2015 at 5:59:37.549219 pm'!

!ColorForm class methodsFor: 'instance creation' stamp: 'jmv 1/9/2015 08:39'!
grayScaleExtent: aPoint
	"Answer a ColorForm for storing 8bpp gray scale. (do not include any slot for transparent)"

	| grays result |
	result _ self extent: aPoint depth: 8.
	grays _ (0 to: 255) collect: [ :brightness | Color gray: brightness asFloat / 255.0].
	result colors: grays.
	^result! !

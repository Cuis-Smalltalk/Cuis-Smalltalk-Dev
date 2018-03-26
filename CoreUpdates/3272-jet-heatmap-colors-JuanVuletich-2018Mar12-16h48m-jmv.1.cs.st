'From Cuis 5.0 of 7 November 2016 [latest update: #3271] on 12 March 2018 at 4:50:33 pm'!

!Color class methodsFor: 'instance creation' stamp: 'jmv 3/7/2018 10:37:22'!
jet: fraction
	"Answer a suitable color for a HeatMap using the 'jet' color scheme.
	See https://en.wikipedia.org/wiki/Heat_map
	
	Color showColors: ((0.0 to: 1.0 count: 100) collect: [ :f | Color jet: f ])
	"
	
	| hue |
	hue _ Color blue hue interpolateTo: Color red hue at: fraction.
	^ Color h: hue s: 1 v: 1! !


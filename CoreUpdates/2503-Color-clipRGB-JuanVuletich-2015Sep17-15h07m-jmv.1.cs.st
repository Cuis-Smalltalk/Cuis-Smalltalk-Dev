'From Cuis 4.2 of 25 July 2013 [latest update: #2501] on 17 September 2015 at 3:53:24.117989 pm'!

!Color class methodsFor: 'instance creation' stamp: 'jmv 9/17/2015 15:22'!
clipR: r g: g b: b
	"Return a color with the given r, g, and b components in the range [0.0..1.0].
	Clip if necessary"

	^ self 
		r: (r min: 1.0 max: 0.0)
		g: (g min: 1.0 max: 0.0)
		b: (b min: 1.0 max: 0.0)! !


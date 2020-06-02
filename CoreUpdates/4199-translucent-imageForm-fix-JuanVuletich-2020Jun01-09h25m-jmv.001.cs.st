'From Cuis 5.0 [latest update: #4195] on 1 June 2020 at 9:26:30 am'!

!Form methodsFor: 'other' stamp: 'jmv 5/31/2020 19:00:18'!
divideByAlpha
	"Divide each pixel by its alpha. Needed after using rule 24 (alphaBlend) on translucent forms."

	| v a r g b |
	depth = 32 ifFalse: [^self].
	1 to: bits size do: [ :i |
		v := bits at: i.
		a := v bitShift: -24.
		a = 0 ifFalse: [
			r := ((v bitShift: -16) bitAnd: 255) * 255 // a.
			g := ((v bitShift: -8) bitAnd: 255) * 255 // a.
			b := (v bitAnd: 255) * 255 // a.
			bits at: i put: (a bitShift: 24) + (r bitShift: 16) + (g bitShift: 8) + b]].! !


!Morph methodsFor: 'drawing' stamp: 'jmv 5/31/2020 19:01:47'!
imageForm: depth
	| canvas |
	canvas _ BitBltCanvas depth: depth over: (self morphPosition extent: self morphExtent).
	canvas fullDraw: self.
	^ canvas form divideByAlpha! !

!Morph methodsFor: 'drawing' stamp: 'jmv 5/31/2020 19:01:52'!
imageForm: extent depth: depth
	| canvas |
	canvas _ BitBltCanvas depth: depth over: (self morphPosition extent: (self morphExtent min: extent)).
	canvas fullDraw: self.
	^ canvas form divideByAlpha! !


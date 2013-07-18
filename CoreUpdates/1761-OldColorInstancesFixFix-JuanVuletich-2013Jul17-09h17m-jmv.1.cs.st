'From Cuis 4.1 of 12 December 2012 [latest update: #1760] on 17 July 2013 at 9:45:36.251827 am'!

!Color methodsFor: 'private' stamp: 'jmv 7/17/2013 09:43'!
fix
	"
	Color allInstancesDo: [ :c | c fix ].
	TranslucentColor allInstancesDo: [ :c | c fix ].
	"
	| r g b componentMask redShift greenShift componentMax |
	floatRGB ifNil: [
		componentMask _ 1023.
		componentMax _ 1023.0.
		redShift _ 20.
		greenShift _ 10.
		r _ (rgb bitShift: 0 - redShift) bitAnd: componentMask.
		g _ (rgb bitShift: 0 - greenShift) bitAnd: componentMask.
		b _  rgb bitAnd: componentMask.
		floatRGB _ FloatArray
			with: r asFloat / componentMax
			with: g asFloat / componentMax
			with: b asFloat / componentMax.
		colorSpace _ colorSpace ifNil: [ #sRGB ]]! !


!TranslucentColor methodsFor: 'private' stamp: 'jmv 7/17/2013 09:43'!
fix

	floatRGB ifNil: [
		super fix.
		floatAlpha _ alpha asFloat / 255.0 ]! !


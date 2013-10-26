'From Cuis 4.0 of 21 April 2012 [latest update: #1364] on 11 August 2012 at 6:37 pm'!

!FFT methodsFor: 'testing' stamp: 'jmv 8/10/2012 23:35'!
plot: samples in: rect
	"Throw-away code just to check out a couple of examples"
	| min max x dx pen y |
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	min _ 1.0e30.  max _ -1.0e30.
	samples do: [ :v |
		min _ min min: v.
		max _ max max: v].
	pen _ Pen new.
	pen up.
	y _ (max/(max-min) * rect height + rect top).
	pen goto: rect left@ y.
	pen down.
	pen goto: rect right@ y.
	pen up.
	x _ rect left.
	dx _ rect width asFloat / samples size.
	samples do: [ :v |
		y _ (max-v) / (max-min) * rect height asFloat.
		pen goto: x asInteger @ (rect top + y asInteger).
		pen down.
		x _ x + dx].
	max printString displayOn: Display at: (x+2) @ (rect top-9).
	min printString displayOn: Display at: (x+2) @ (rect bottom - 9)! !


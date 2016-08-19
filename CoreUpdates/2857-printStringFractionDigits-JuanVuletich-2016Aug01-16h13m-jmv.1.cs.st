'From Cuis 4.2 of 25 July 2013 [latest update: #2850] on 1 August 2016 at 5:10:38 pm'!

!Number methodsFor: 'printing' stamp: 'jmv 8/1/2016 16:13:02'!
printStringFractionDigits: placesDesired
	"Print a representation of the receiver on aStream in decimal notation with prescribed number of places after decimal separator.
	Float pi printStringFractionDigits: 4
	"
	^ String streamContents: [ :strm | self printOn: strm fractionDigits: placesDesired ]! !


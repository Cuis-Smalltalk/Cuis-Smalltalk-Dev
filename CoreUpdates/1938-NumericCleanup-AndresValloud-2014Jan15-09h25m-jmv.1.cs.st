'From Cuis 4.2 of 25 July 2013 [latest update: #1937] on 15 January 2014 at 9:26:29.57953 am'!

!Integer methodsFor: 'arithmetic' stamp: 'sqr 1/14/2014 01:30'!
reciprocalModulo: mod
	"Answer an integer x such that self * x \\ n = 1, with 0 < x < n, or nil if x does not exist.
	The algorithm is a non extended euclidean modular inversion called NINV.
	It is described in this article:
		'Using an RSA Accelerator for Modular Inversion'
	by Martin Seysen. See http://www.iacr.org/archive/ches2005/017.pdf"

	| receiver u v f fPlusN b result result2 |
	mod > 1 ifFalse: [^nil].
	receiver := (self >= mod or: [self < 0])
		ifTrue: [self \\ mod] ifFalse: [self].
	b := mod highBit + 1.
	f := 1 bitShift: b.
	v := (receiver bitShift: b) + 1.
	u := mod bitShift: b.
	fPlusN := f + mod.
	[v >= fPlusN] whileTrue:
		[v := u \\\ (u := v)].
	result := v - f.
	result2 := result + mod.
	result2 > 0 ifFalse: [^nil].
	^result positive
		ifTrue: [result]
		ifFalse: [result2]! !


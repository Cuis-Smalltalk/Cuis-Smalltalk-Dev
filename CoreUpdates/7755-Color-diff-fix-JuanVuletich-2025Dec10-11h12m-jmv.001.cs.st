'From Cuis7.5 [latest update: #7753] on 10 December 2025 at 11:12:53 am'!

!Color methodsFor: 'equality' stamp: 'jmv 10/Dec/2025 10:55:54'!
diff: theOther
	"Returns a number between 0.0 and 1.0.
	Color gray diff: Color red
	"
	^(self - theOther ) brightness max: (theOther -self ) brightness! !


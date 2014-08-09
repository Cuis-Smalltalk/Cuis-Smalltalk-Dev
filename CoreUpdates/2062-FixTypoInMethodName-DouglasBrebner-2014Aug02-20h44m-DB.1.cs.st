'From Cuis 4.2 of 25 July 2013 [latest update: #2055] on 2 August 2014 at 8:48:08.647259 pm'!

!MatrixTransform2x3 methodsFor: 'initialization' stamp: 'DB 8/2/2014 20:44'!
setIdentity
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	self
		a11: 1.0; a12: 0.0; a13: 0.0;
		a21: 0.0; a22: 1.0; a23: 0.0.! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'DB 8/2/2014 20:45'!
identity
	^self new setIdentity! !

!methodRemoval: MatrixTransform2x3 #setIdentiy!
MatrixTransform2x3 removeSelector: #setIdentiy!

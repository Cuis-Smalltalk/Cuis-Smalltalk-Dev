'From Cuis 5.0 [latest update: #3888] on 29 September 2019 at 1:08:13 pm'!

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 9/29/2019 12:48:46'!
instancesMustBeOfSpecificSize
	"Some subclasses create instances of a specific size, and answer true"
	^self numElements ~= 0! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 9/29/2019 12:47:53'!
numElements
	"Some subclasses create instances of a specific size, and a non-zero number"
	^0! !


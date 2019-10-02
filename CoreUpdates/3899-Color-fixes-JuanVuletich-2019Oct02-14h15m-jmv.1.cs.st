'From Cuis 5.0 [latest update: #3898] on 2 October 2019 at 2:16:17 pm'!

!Color methodsFor: 'object serialization' stamp: 'jmv 6/22/2017 12:54:10'!
convertToCurrentVersion: varDict refStream: smartRefStrm

	"subclasses should implement if they wish to convert old instances to modern ones"
	self size = 0 ifTrue: [
		^ Color new copyFrom: (varDict at: 'floatRGB') ].
	^ self! !


!Color methodsFor: 'private' stamp: 'jmv 10/2/2019 13:53:40'!
setRed: r green: g blue: b range: range
	"Initialize this color's r, g, and b components to the given values in the range [0..r]."

	self basicSetRed: r/range green: g/range blue: b/range.
	self clipToValidValues! !

!methodRemoval: Color #*=!
Color removeSelector: #*=!
!methodRemoval: Color #+=!
Color removeSelector: #+=!
!methodRemoval: Color #-=!
Color removeSelector: #-=!
!methodRemoval: Color #/=!
Color removeSelector: #/=!

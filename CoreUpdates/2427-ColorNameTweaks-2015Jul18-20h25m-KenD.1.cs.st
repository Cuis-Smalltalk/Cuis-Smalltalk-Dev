'From Cuis 4.2 of 25 July 2013 [latest update: #2404] on 18 July 2015 at 8:49:46.349012 pm'!

!Color methodsFor: 'other' stamp: 'KenD 7/18/2015 20:41'!
colorName
	"Return this color's name, or nil if it has no name. Only returns a name if it exactly matches the named color."

	^(Color colorNamesDict keyAtValue: self  ifAbsent: [nil])
! !

!Color methodsFor: 'printing' stamp: 'KenD 7/18/2015 20:44'!
printString
	"Answer a String whose characters are a description of the receiver."
	
	^ String streamContents: [ :stream | self printOn: stream ]! !


!Color methodsFor: 'other' stamp: 'KenD 7/18/2015 20:46'!
name
	"Return this color's name, or description if unnamed."

	^ self printString
! !

!Color methodsFor: 'printing' stamp: 'KenD 7/18/2015 20:42'!
printOn: aStream
	 self colorName ifNotNil: [ :name |
		^ aStream
			nextPutAll: 'Color ';
			nextPutAll: name].
	self storeOn: aStream.
! !


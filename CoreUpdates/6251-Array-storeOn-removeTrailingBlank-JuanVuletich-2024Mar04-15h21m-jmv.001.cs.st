'From Cuis6.3 [latest update: #6248] on 4 March 2024 at 3:22:23 pm'!

!Array methodsFor: 'printing' stamp: 'jmv 3/3/2024 11:51:39'!
storeOn: aStream 
	"Use the literal form if possible."

	self isLiteral
		ifTrue: [
			aStream nextPut: $#; nextPut: $(.
			self
				do: [ :element | 
					element storeOn: aStream ]
				separatedBy: [.
					aStream space ].
			aStream nextPut: $)]
		ifFalse: [super storeOn: aStream]! !


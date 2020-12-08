'From Cuis 5.0 [latest update: #4484] on 8 December 2020 at 8:56:57 am'!

!ColorForm methodsFor: 'private' stamp: 'jmv 12/8/2020 08:44:09'!
ensureColorArrayExists
	"Return my color palette."

	| colorsToUse |
	colors ifNil: [
		self depth > 8 ifTrue: [^ self error: 'ColorForms only support depths up to 8 bits'].
		colorsToUse _ Color indexedColors copyFrom: 1 to: (1 bitShift: self depth).
		"Note: zero is transparent except when depth is one-bit deep"
		self depth > 1 ifTrue: [
			colorsToUse at: 1 put: Color transparent ].
		self colors: colorsToUse].
! !


'From Cuis 6.0 [latest update: #5781] on 29 April 2023 at 8:02:18 pm'!

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 4/29/2023 19:57:15'!
normalCursorWithMask
	"Cursor initNormalWithMask.  Cursor normalCursor show"
	"Next two lines work simply for any cursor..."
	"
	self initNormal.
	NormalCursor _ CursorWithMask derivedFrom: NormalCursor.
	"
	"But for a good looking cursor, you have to tweak things..."
	^ ((CursorWithMask
		small1BitExtent: `16@16`
		pixelBits: #(
			2r0000000000000000
			2r0100000000000000
			2r0110000000000000
			2r0111000000000000
			2r0111100000000000
			2r0111110000000000
			2r0111111000000000
			2r0111111100000000
			2r0111111110000000
			2r0111110000000000
			2r0110110000000000
			2r0100011000000000
			2r0000011000000000
			2r0000001100000000
			2r0000001100000000
			2r0000000000000000)
		offset: `-1@ -1`)
	setMaskForm: 
		(Form
			small1BitExtent: `16@16`
			pixelBits: #(
				2r1100000000000000
				2r1110000000000000
				2r1111000000000000
				2r1111100000000000
				2r1111110000000000
				2r1111111000000000
				2r1111111100000000
				2r1111111110000000
				2r1111111111000000
				2r1111111111100000
				2r1111111000000000
				2r1110111100000000
				2r1100111100000000
				2r1000011110000000
				2r0000011110000000
				2r0000001110000000)
			offset: `0@0`))! !


!CursorWithMask class methodsFor: 'as yet unclassified' stamp: 'jmv 4/29/2023 19:57:21'!
derivedFrom: aForm      "Cursor initNormalWithMask.  Cursor normalCursor show"
	"aForm is presumably a cursor"
	| cursor mask ext |
	ext := aForm extent.
	cursor := self extent: ext.
	cursor copy: (1@1 extent: ext) from: `0@0` in: aForm rule: Form over.
	mask := Form extent: ext.
	(1@1) eightNeighbors do:
		[:p | mask copy: (p extent: ext) from: `0@0` in: aForm rule: Form under].
	cursor setMaskForm: mask.
	cursor offset: ((aForm offset - (1@1)) max: ext negated).
	^ cursor! !


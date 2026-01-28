'From Cuis7.7 [latest update: #7817] on 28 January 2026 at 9:35:43 am'!

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 28/Jan/2026 09:34:39'!
normalCursorBig
	"Answer the default top left pointing arrow cursor.
	This is an anti aliased big version drawn with Vector Graphics"

	| form cursor |
	form:= Form extent: 32@32 depth: 32.
	(VectorEngineWholePixel onForm: form subPixelAntiAliasing: false)
		enableTargetTranslucency;
		geometryTransformation: (AffineTransformation withScale: 1.8 position: 1@1);
		strokeWidth: 1.0 color: Color white fillColor: Color black do: [ :engine |
			engine
				pathSequence: `#[
					0.0 0.0 0.0
					1.0 10.0 10.0
					1.0 5.56 10.0
					1.0 7.67 15.0
					1.0 6.67 16.0
					1.0 5.27 16.0
					1.0 3.16 11.0
					1.0 0.0 14.0
					1.0 0.0 0.0
					] asFloat32Array`
				].
	cursor := CursorWithAlpha extent: form extent depth: 32.
	form displayOn: cursor.
	cursor offset: -2@ -1.
	cursor offset: -3@ -3.
	cursor preMultiplyAlpha.
	^cursor! !

!Cursor class methodsFor: 'instance creation' stamp: 'jmv 28/Jan/2026 09:34:24'!
normalCursorSmall
	"Answer the default top left pointing arrow cursor.
	This is a classic small version witn 1 bit pixels and mask."

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


!Cursor class methodsFor: 'instance creation' stamp: 'jmv 28/Jan/2026 09:32:24'!
normalCursor
	"Answer the default top left pointing arrow cursor"

	^ (Preferences at: #biggerCursors)
		ifFalse: [ self normalCursorSmall ]
		ifTrue: [ self normalCursorBig ]! !


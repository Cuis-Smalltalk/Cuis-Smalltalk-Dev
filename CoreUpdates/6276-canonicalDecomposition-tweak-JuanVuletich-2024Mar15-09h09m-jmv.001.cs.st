'From Cuis6.3 [latest update: #6275] on 15 March 2024 at 9:11:02 am'!

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 3/15/2024 09:09:46'!
canonicalComposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFC form 'Normalization Form Composed'.

	The canonical composition is cached. Therefor it doesn't need to be computed every time it is needed.
	This makes it actually cheaper than #canonicalDecomposition!!

	UnicodeString fromUtf8Bytes: #[195 162].
	(UnicodeString fromUtf8Bytes: #[195 162]) asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[195 162]) canonicalDecomposition asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[195 162]) canonicalComposition asUtf8Bytes

	UnicodeString fromUtf8Bytes: #[97 204 130].
	(UnicodeString fromUtf8Bytes: #[97 204 130]) asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[97 204 130]) canonicalDecomposition asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[97 204 130]) canonicalComposition asUtf8Bytes
	"

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCanonicalCompositionAndHash ].

	^hash isNumber
		ifTrue: [
			"If 'hash' is a number it means we are already in canonicalComposition form (NFC)."
			self ]
		ifFalse: [
			"If 'hash' is not a number, it actually contains our #canonicalComposition."
			hash ].! !


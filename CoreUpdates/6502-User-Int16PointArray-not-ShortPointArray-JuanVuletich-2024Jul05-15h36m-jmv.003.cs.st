'From Cuis7.1 [latest update: #6500] on 5 July 2024 at 3:46:50 pm'!

!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 7/5/2024 15:46:15'!
readAllTrueTypeFontsIn: aDirectoryEntry
	"
	TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory / 'DejaVu' / 'DejaVuSans'
	"
	| readFamilyNames |
	readFamilyNames := TTFontDescription readTrueTypeFontsIn: aDirectoryEntry.
	readFamilyNames do: [ :familyName |
		| ttDescriptions baseTTDescription family |
		ttDescriptions := TTFontDescription withFamilyName: familyName.
		baseTTDescription := ttDescriptions at: 0.
		family := TrueTypeFontFamily baseTTFontDescription: baseTTDescription.
		ttDescriptions do: [ :ttfDesc |
			ttfDesc emphasis = 0 ifFalse: [
				family addTTFontDescription: ttfDesc ]].
		FontFamily addFamily: family ].
	^readFamilyNames! !


!TTGlyph methodsFor: 'private-initialization' stamp: 'jmv 7/5/2024 15:36:46'!
initializeContours: numContours with: endPoints
	"Initialize the contours for creation of the glyph."
	| startPt pts endPt |
	contours := Array new: numContours.
	startPt := -1.
	1 to: numContours do:[:i|
		endPt := endPoints at: i.
		pts := Int16PointArray new: endPt - startPt.
		contours at: i put: (TTContourConstruction on: pts).
		startPt := endPt].! !


"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory / 'DejaVu' / 'DejaVuSans'!


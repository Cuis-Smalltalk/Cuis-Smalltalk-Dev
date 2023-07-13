'From Cuis 6.0 [latest update: #5910] on 12 July 2023 at 6:52:05 pm'!
!classDefinition: #TrueTypeFont category: 'Graphics-TrueType'!
AbstractFont subclass: #TrueTypeFont
	instanceVariableNames: 'ttFontDescription pointSize emphasis glyphFormsByUtf8 family baseFont derivativeFonts xxx '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #Sample10PythagorasTree category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample10PythagorasTree
	instanceVariableNames: 'depthLimit angle colors xxx '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #VectorCanvas category: #'Morphic-Support'!
AbstractVectorCanvas subclass: #VectorCanvas
	instanceVariableNames: 'boundsFinderCanvas auxBitBltEngine attachedCanvas xxx '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #FormGlyph category: 'Graphics-TrueType'!
Object subclass: #FormGlyph
	instanceVariableNames: 'form leftOffset topOffset advanceWidth deltaXToCenter xxx '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #TTFontDescription category: 'Graphics-TrueType'!
Object subclass: #TTFontDescription
	instanceVariableNames: 'folderName contourDataForUtf8 contourDataIndexesByUtf8 kernPairs copyright familyName fullName subfamilyName uniqueName versionName postscriptName trademark bounds unitsPerEm ascent descent lineGap letterMTopSideBearing xxx '
	classVariableNames: 'Descriptions '
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #TrueTypeFont category: 'Graphics-TrueType'!
AbstractFont subclass: #TrueTypeFont
	instanceVariableNames: 'ttFontDescription pointSize emphasis glyphFormsByUtf8 family baseFont derivativeFonts'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #Sample10PythagorasTree category: 'Morphic-Examples'!
PlacedMorph subclass: #Sample10PythagorasTree
	instanceVariableNames: 'depthLimit angle colors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!
!classDefinition: #VectorCanvas category: #'Morphic-Support'!
AbstractVectorCanvas subclass: #VectorCanvas
	instanceVariableNames: 'boundsFinderCanvas auxBitBltEngine attachedCanvas'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
!classDefinition: #FormGlyph category: 'Graphics-TrueType'!
Object subclass: #FormGlyph
	instanceVariableNames: 'form leftOffset topOffset advanceWidth deltaXToCenter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-TrueType'!
!classDefinition: #TTFontDescription category: 'Graphics-TrueType'!
Object subclass: #TTFontDescription
	instanceVariableNames: 'folderName contourDataForUtf8 contourDataIndexesByUtf8 kernPairs copyright familyName fullName subfamilyName uniqueName versionName postscriptName trademark bounds unitsPerEm ascent descent lineGap letterMTopSideBearing'
	classVariableNames: 'Descriptions'
	poolDictionaries: ''
	category: 'Graphics-TrueType'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk organization renameCategory: 'Graphics-PathCommands' to: 'xGraphics-PathCommands'.
Smalltalk organization renameCategory: 'Graphics-TrueType' to: 'xGraphics-TrueType'.
Smalltalk organization renameCategory: 'Morphic-Examples' to: 'xMorphic-Examples'.
Smalltalk organization renameCategory: 'xGraphics-PathCommands' to: 'Graphics-PathCommands' asByteStringIfAscii.
Smalltalk organization renameCategory: 'xGraphics-TrueType' to: 'Graphics-TrueType' asByteStringIfAscii.
Smalltalk organization renameCategory: 'xMorphic-Examples' to: 'Morphic-Examples' asByteStringIfAscii.
BasicClassOrganizer allSubInstancesDo: [ :categorizer | categorizer commentStamp class == UnicodeString  ifTrue: [categorizer commentStamp: categorizer commentStamp asByteStringIfAscii ]].
!


'From Cuis 6.0 [latest update: #5483] on 16 September 2022 at 9:36:28 am'!

!FontFamily class methodsFor: 'ui' stamp: 'jmv 9/16/2022 09:33:06'!
readIfNeeded: nameOrDirectory
	"Read from disk if not in memory"

	(FontFamily familyNamed: nameOrDirectory)
		ifNotNil: [ :it | ^it ].
	^FontFamily availableFamilies values
		detect: [ :any | any folderName = nameOrDirectory name ]
		ifNone: [
			FontFamily familyNamed: ((Smalltalk at: #TrueTypeFontFamily)
				readAllTrueTypeFontsIn: nameOrDirectory) anyOne ]! !


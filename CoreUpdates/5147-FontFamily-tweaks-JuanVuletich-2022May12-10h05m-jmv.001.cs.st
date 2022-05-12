'From Cuis 6.0 [latest update: #5146] on 12 May 2022 at 10:27:09 am'!

!FontFamily class methodsFor: 'ui' stamp: 'jmv 5/12/2022 10:05:27'!
readIfNeeded: nameOrDirectory
	"Read from disk if not in memory"

	(FontFamily familyNamed: nameOrDirectory)
		ifNotNil: [ :it | ^it ].
	^FontFamily availableFamilies values
		detect: [ :any | any folderName = nameOrDirectory name ]
		ifNone: [
			FontFamily familyNamed: ((Smalltalk at: #TrueTypeFontFamily) read: nameOrDirectory) anyOne ]! !


!FontFamily class methodsFor: 'ui' stamp: 'jmv 5/12/2022 10:06:50'!
promptUser
	"Present a menu of font families, answer selection.
	FontFamily promptUser
	"
	^self promptUserFolders ifNotNil: [ :selectedNameOrDirectory |
		self readIfNeeded: selectedNameOrDirectory ].! !

!FontFamily class methodsFor: 'ui' stamp: 'jmv 5/12/2022 10:12:56'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	| fontFamily |
	self promptUserFolders ifNotNil: [ :selectedNameOrDirectory |
		(Feature require: 'VectorGraphics') ifTrue: [
			(Smalltalk at: #VectorEngineWithPlugin) isPluginAvailable ifFalse: [
				Feature require: 'VectorEngineInSmalltalk' ].
			UISupervisor whenUIinSafeState: [
				fontFamily _ self readIfNeeded: selectedNameOrDirectory.
				Preferences setDefaultFont: fontFamily familyName ]]].! !


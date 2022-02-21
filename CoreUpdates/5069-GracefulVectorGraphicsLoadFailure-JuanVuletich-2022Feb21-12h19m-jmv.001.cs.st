'From Cuis 6.0 [latest update: #5067] on 21 February 2022 at 12:25:58 pm'!

!FontFamily class methodsFor: 'file read write' stamp: 'jmv 2/21/2022 12:21:16'!
readAdditionalTrueTypeFonts
	(Feature require: 'VectorGraphics') ifTrue: [
		(Smalltalk at: #VectorEngineWithPlugin) isPluginAvailable ifFalse: [
			Feature require: 'VectorEngineInSmalltalk' ].
		UISupervisor whenUIinSafeState: [
			Smalltalk at: #TrueTypeFontFamily ifPresent: [ :cls | cls readAdditionalFonts ]]].! !

!FontFamily class methodsFor: 'ui' stamp: 'jmv 2/21/2022 12:22:03'!
promptUserAndSetDefault
	"Present a menu of available font families, and if one is chosen, change to it.
	FontFamily promptUserAndSetDefault
	"
	| selectedNameOrDirectory fontFamily |
	selectedNameOrDirectory _ self promptUserFolders.
	selectedNameOrDirectory isNil ifTrue: [ ^nil ].
	(FontFamily familyNamed: selectedNameOrDirectory)
		ifNotNil: [ :it | ^Preferences setDefaultFont: it familyName ].
	(Feature require: 'VectorGraphics') ifTrue: [
		(Smalltalk at: #VectorEngineWithPlugin) isPluginAvailable ifFalse: [
			Feature require: 'VectorEngineInSmalltalk' ].
		UISupervisor whenUIinSafeState: [
			fontFamily _ FontFamily availableFamilies values
				detect: [ :any | any folderName = selectedNameOrDirectory name ]
				ifNone: [
					FontFamily familyNamed: ((Smalltalk at: #TrueTypeFontFamily) read: selectedNameOrDirectory) anyOne ].
			Preferences setDefaultFont: fontFamily familyName ]].! !


'From Cuis 6.0 [latest update: #5159] on 13 May 2022 at 4:46:22 pm'!

!FontFamily class methodsFor: 'ui' stamp: 'hlsf 5/13/2022 16:45:46'!
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
				fontFamily includesAscii
					ifTrue: [PreferenceNG setDefaultFont: fontFamily familyName ]
					ifFalse: [self inform: 'Selected font does not include ASCII characters. Can not be set as default.' ]]]].! !


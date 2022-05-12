'From Cuis 6.0 [latest update: #5147] on 12 May 2022 at 10:40:53 am'!

!StrikeFontFamily methodsFor: 'testing' stamp: 'jmv 5/12/2022 10:36:05'!
includesAscii
	"Answer true if we include the basic ASCII characters.
	If not, we are not suitable, for instance, for displaying Smalltalk code."
	^true! !


!FontFamily class methodsFor: 'ui' stamp: 'jmv 5/12/2022 10:37:23'!
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
					ifTrue: [Preferences setDefaultFont: fontFamily familyName ]
					ifFalse: [self inform: 'Selected font does not include ASCII characters. Can not be set as default.' ]]]].! !


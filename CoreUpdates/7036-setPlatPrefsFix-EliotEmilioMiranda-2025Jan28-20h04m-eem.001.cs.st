'From Cuis7.3 [latest update: #7035] on 28 January 2025 at 8:09:26 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'eem 1/28/2025 20:04:34'!
setPlatformPreferences
	"Set some platform specific preferences on system startup"
	| platform specs |	

	(Preferences at: #automaticPlatformSettings) ifFalse:[^self].
	platform := self platformName.
	specs := 	#(	
					(soundStopWhenDone false)
					(soundQuickStart false)
			).
	platform = 'Win32' ifTrue:[
		specs := #(	
					(soundStopWhenDone true)
					(soundQuickStart false)
				)].
	platform = 'Mac OS' ifTrue:[
		specs := #(	
					(soundStopWhenDone false)
					(soundQuickStart true)
				)].
	specs do: [:tuple |
		Preferences name: tuple first category: #system value: tuple last ]! !


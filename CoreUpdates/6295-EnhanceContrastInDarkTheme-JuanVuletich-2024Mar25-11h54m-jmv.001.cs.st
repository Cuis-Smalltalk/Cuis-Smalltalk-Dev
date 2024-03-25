'From Cuis6.3 [latest update: #6294] on 25 March 2024 at 11:55:16 am'!

!DarkTheme methodsFor: 'colors' stamp: 'jmv 3/25/2024 10:18:15'!
defaultWindowColor
	^ `Color hue: 212 chroma: 0.015 luminance: 0.15`! !

!DarkTheme methodsFor: 'colors' stamp: 'jmv 3/25/2024 11:24:12'!
paneBackgroundFrom: aColor
	^ aColor alphaMixed: 0.4 with: Color black! !


'From Cuis 6.0 [latest update: #5166] on 15 May 2022 at 12:22:37 pm'!

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 5/14/2022 11:42:57'!
allPreferences
	^ ThePreferences 
	! !

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 5/15/2022 12:17:37'!
selectCategory: aCategory
" I select the preferences of the given category "
	^ ThePreferences values select: [:aPreference | aPreference category == aCategory ]! !


!PreferenceNG class methodsFor: 'class initialization' stamp: 'hlsf 5/15/2022 12:21:27'!
initialize
    	ThePreferences ifNil: [ThePreferences _ Dictionary new].
	#(gui font programming system) do: [: category | 
		(self perform: (#pref, category capitalized) asSymbol) do: [:aPrefArray |
			self installDefault: aPrefArray in: category] ].
	self installHaloPreferencesWith: self iconicHaloSpecifications.
	self installMiscPreferences.
	self defaultFontSize: #standardFonts.
	self setDefaultFont: FontFamily defaultFamilyName.! !

!PreferenceNG class methodsFor: 'fileIn/Out' stamp: 'hlsf 5/15/2022 12:21:53'!
saveCategory: categorySymbol to: aFileEntry
	| myPref |
	myPref _ self selectCategory: categorySymbol.
	SmartRefStream 
		dumpOnFile: aFileEntry 
		object: 	(myPref collect: [:aPref | aPref name -> aPref ]) asDictionary! !

!methodRemoval: PreferenceNG class #select: stamp: 'hlsf 5/15/2022 12:17:40'!
PreferenceNG class removeSelector: #select:!
!methodRemoval: PreferenceNG class #all stamp: 'jmv 5/15/2022 12:16:20'!
PreferenceNG class removeSelector: #all!
PreferenceNG initialize!

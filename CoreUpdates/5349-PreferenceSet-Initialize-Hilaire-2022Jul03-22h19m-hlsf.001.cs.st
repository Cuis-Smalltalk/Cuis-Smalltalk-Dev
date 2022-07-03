'From Cuis 6.0 [latest update: #5347] on 3 July 2022 at 10:20:12 pm'!

!PreferenceSet methodsFor: 'initialization' stamp: 'hlsf 7/3/2022 22:15:45'!
initialize
	super initialize.
	contents _ Dictionary new.! !


!PreferenceSet reorganize!
('instance creation' name:category:value: name:description:category:type:value:)
('accessing' allPreferences at: at:put: categories instanceAt: openPreferencesInspector selectCategory:)
('error handling' doesNotUnderstand:)
('fileIn/Out' loadFrom: save:to: saveAllTo: saveCategory:to:)
('initialization' initialize)
!


'From Cuis7.7 [latest update: #7890] on 30 April 2026 at 3:15:02 pm'!

!CodeFile methodsFor: 'change record types' stamp: 'jmv 30/Apr/2026 10:02:06'!
classRemoval: aClassDeletionChange
	
	^self classDefinition: aClassDeletionChange! !

!CodeFile methodsFor: 'reading' stamp: 'jmv 30/Apr/2026 11:46:08'!
buildFrom: changes informingTo: barBlock 

	changes withIndexDo: [ :change :anIndex | 
		barBlock value: anIndex.
		change performOn: self ].! !


!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 30/Apr/2026 11:46:36'!
provides: aFeatureProvidesRecord
	featureSpec provides: aFeatureProvidesRecord feature.! !

!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 30/Apr/2026 11:46:47'!
requires: aFeatureRequiresRecord
	featureSpec requires: aFeatureRequiresRecord feature.! !


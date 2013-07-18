'From Cuis 4.1 of 12 December 2012 [latest update: #1755] on 16 July 2013 at 5:46:39.892975 pm'!

!Feature methodsFor: 'testing' stamp: 'jmv 7/16/2013 17:45'!
satisfies: requirement
	"Does this provided Feature satisfy the required Feature?"
	^ name = requirement name and: [
		requirement version isNil or: [
			version = requirement version and: [
				requirement revision isNil or: [ revision >= requirement revision ]]]]! !


!Feature class methodsFor: 'accessing' stamp: 'jmv 7/16/2013 17:43'!
addDirectoryToLookForPackages: aFileDirectory

	PlacesToLookForPackages 
		ifNil: [ PlacesToLookForPackages := OrderedCollection new ].
		
	PlacesToLookForPackages add: aFileDirectory! !

!Feature class methodsFor: 'class initialization' stamp: 'jmv 7/16/2013 17:42'!
initialize

	"
	Feature initialize
	"

	PlacesToLookForPackages := OrderedCollection new.
						
	self addDirectoryToLookForPackages: FileDirectory default.
			
	self addDirectoryToLookForPackages: 
		( FileDirectory default directoryNamed: 'Packages' )! !


!FeatureSpec class methodsFor: 'instance creation' stamp: 'jmv 7/16/2013 17:44'!
provides: providedFeature requires: requiredFeaturesSet

	^self new provides: providedFeature requires: requiredFeaturesSet asSet! !

Feature initialize!

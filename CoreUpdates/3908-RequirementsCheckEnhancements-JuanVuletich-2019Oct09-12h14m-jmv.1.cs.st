'From Cuis 5.0 [latest update: #3907] on 9 October 2019 at 12:17:11 pm'!

!FeatureRequirementUnsatisfied methodsFor: 'exceptionDescription' stamp: 'jmv 10/9/2019 11:57:30'!
defaultAction
	"The default action taken if the exception is signaled."

	self messageText print.
	PopUpMenu inform: 
		self messageText, 
		String newLineString, String newLineString,
		'You can view loaded Packages and their requirements via',
		String newLineString,
		'        World menu > Open.. > Installed Packages',
		String newLineString
	
! !


!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 10/9/2019 12:14:51'!
findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageFileName entry |
	pathName ifNotNil: [
		pathName asFileEntry exists ifTrue: [ ^ true ]].
	packageFileName _ self packageFileName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :directory |
		entry _ directory // packageFileName.
		entry exists ifTrue: [
			pathName _ entry pathName.
			self checkRequirement ifTrue: [ ^true ].
			pathName _ nil]].
	^ false! !

!FeatureRequirement methodsFor: 'testing' stamp: 'jmv 10/9/2019 12:16:13'!
checkRequirement
	"Answer if I am satisfied by package found at pathName"
	
	pathName asFileEntry readStreamDo: [ :stream | | fSpec |
		fSpec := ((CodePackageFile buildFileStream: stream) featureSpec).
		^ fSpec notNil and: [fSpec provides satisfies: self]]
							 
! !


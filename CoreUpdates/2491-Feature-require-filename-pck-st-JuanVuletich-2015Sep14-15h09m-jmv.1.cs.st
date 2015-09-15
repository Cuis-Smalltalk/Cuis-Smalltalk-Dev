'From Cuis 4.2 of 25 July 2013 [latest update: #2490] on 14 September 2015 at 3:37:59.43651 pm'!

!Feature class methodsFor: 'convenience' stamp: 'jmv 9/14/2015 15:29'!
require: featureNameOrFilename
	"
	Feature require: 'StyledTextInstaller'
	Feature require: 'Sound'
	Feature require: 'Tests'
	"
	| req ext fileEntry |
	ext _ '.pck.st'.
	((featureNameOrFilename endsWith: ext) and: [
				fileEntry _ featureNameOrFilename asFileEntry.
				fileEntry exists ])
		ifTrue: [
			req _ FeatureRequirement name: (fileEntry name withoutSuffix: ext).
			req pathName: featureNameOrFilename.
			]
		ifFalse: [ req _ FeatureRequirement name: featureNameOrFilename ].
	req require! !


!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 9/14/2015 15:34'!
requirements
	"Answer my requirements"
	
	| packageFile |
	packageFile _ CodePackageFile buildFileStream: pathName asFileEntry readStream.
	^ packageFile requires! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 9/14/2015 15:09'!
findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageFileName |
	pathName ifNotNil: [
		pathName asFileEntry exists ifTrue: [ ^ true ]].
	packageFileName _ self packageFileName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :fileDirectory |
		(fileDirectory fileExists: packageFileName) ifTrue: [
			pathName _ fileDirectory fullNameFor: packageFileName.
			^ true ]].
	^ false! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 9/14/2015 15:37'!
install
	"Preconditions have been satisfied.  Install the required package."

	CodePackageFile basicInstallPackageStream: pathName asFileEntry readStream.

	"No need to have a platform and machine specific path around anymore. It was just for installation. Clear it."
	pathName _ nil! !


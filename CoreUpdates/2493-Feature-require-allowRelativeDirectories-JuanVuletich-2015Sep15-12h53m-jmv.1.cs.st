'From Cuis 4.2 of 25 July 2013 [latest update: #2492] on 15 September 2015 at 12:56:38.845536 pm'!

!Feature class methodsFor: 'convenience' stamp: 'jmv 9/15/2015 11:50'!
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
			req pathName: fileEntry pathName.
			]
		ifFalse: [ req _ FeatureRequirement name: featureNameOrFilename ].
	req require! !


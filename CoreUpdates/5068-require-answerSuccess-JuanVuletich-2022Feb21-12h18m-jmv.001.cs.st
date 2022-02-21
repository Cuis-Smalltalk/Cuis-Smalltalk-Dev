'From Cuis 6.0 [latest update: #5067] on 21 February 2022 at 12:19:35 pm'!

!Feature class methodsFor: 'convenience' stamp: 'jmv 2/21/2022 12:04:19'!
require: featureNameOrFilename
	"
	Answer whether the request is fulfilled.
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
	req isAlreadySatisfied ifFalse: [
		req require ].
	^req isAlreadySatisfied! !


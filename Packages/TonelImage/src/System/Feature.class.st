"
A package has a ProvidedFeature which specifies its version and revision.  See class FeatureSpec.

  name -- a symbol naming the feature
  version -- the integer version/  This increments with each incompatable interface change.
  revision -- the integer revision which increments each time a package is saved.
"
Class {
	#name : 'Feature',
	#superclass : 'Object',
	#instVars : [
		'name',
		'version',
		'revision'
	],
	#category : 'System-Package Support',
	#package : 'System',
	#tag : 'Package Support'
}

{ #category : 'instance creation' }
Feature class >> baseSystemFeature [
	| cuisVersionAndRevision |
	cuisVersionAndRevision := SystemVersion current versionRevision.
	^Feature
		name: #'Cuis-Base' 
		version: cuisVersionAndRevision first
		revision: cuisVersionAndRevision second
]

{ #category : 'instance creation' }
Feature class >> fromTokens: tokens [
	^self 
		name: tokens second
		version: tokens third
		revision: tokens fourth.
]

{ #category : 'instance creation' }
Feature class >> name: aSymbol version: integerVersion revision: integerRevision [

	| newInst |
	newInst := self new.
	newInst name: aSymbol asSymbol version: integerVersion revision: integerRevision.
	
	^ newInst
]

{ #category : 'convenience' }
Feature class >> require: featureNameOrFilename [
	"
	Answer whether the request is fulfilled.
	Feature require: 'StyledTextInstaller'
	Feature require: 'Sound'
	Feature require: 'Tests'
	"
	| req ext fileEntry |
	ext := '.pck.st'.
	((featureNameOrFilename endsWith: ext) and: [
				fileEntry := featureNameOrFilename asFileEntry.
				fileEntry exists ])
		ifTrue: [
			req := FeatureRequirement name: (fileEntry name withoutSuffix: ext).
			req pathName: fileEntry pathName.
			]
		ifFalse: [ req := FeatureRequirement name: featureNameOrFilename ].
	req isAlreadySatisfied ifFalse: [
		req require ].
	^req isAlreadySatisfied
]

{ #category : 'convenience' }
Feature class >> require: featureName version: versionNumber [
	"
	Feature require: 'StyledTextInstaller' version: 1
	Feature require: 'Sound' version: 1
	Feature require: 'Tests' version: 1
	"
	(FeatureRequirement name: featureName version: versionNumber) require
]

{ #category : 'convenience' }
Feature class >> require: featureName version: integerVersion revision: integerRevision [
	"
	Feature require: 'Sound' version: 1 revision: 0
	Feature require: 'Tests' version: 1 revision: 0
	"
	(FeatureRequirement name: featureName version: integerVersion revision: integerRevision) require
]

{ #category : 'convenience' }
Feature class >> requireInformFaiure: featureNameOrFilename [
	"
	Answer whether the request is fulfilled.
	Feature require: 'StyledTextInstaller'
	Feature require: 'Sound'
	Feature require: 'Tests'
	"
	^ [ self require: featureNameOrFilename ]
		on: FeatureRequirementUnsatisfied
		do: [ :ex | PopUpMenu inform: ex messageText. false ]
]

{ #category : 'testing' }
Feature >> isLaterThan: aFeature [
	^self version > aFeature version or: [
		self version = aFeature version and: [
			self revision > aFeature revision ]]
]

{ #category : 'accessing' }
Feature >> name [
	
	^ name
]

{ #category : 'initialization' }
Feature >> name: theName version: theVersion revision: theRevision [

	name     := theName.
	version  := theVersion.
	revision := theRevision
]

{ #category : 'incrementing' }
Feature >> newRevision [
	"Increment revision number"

	revision := revision + 1
]

{ #category : 'incrementing' }
Feature >> newVersion [
	"Increment major version number."

	version := version + 1.
	revision := 0
]

{ #category : 'printing' }
Feature >> printDetailsOn: aStream [

	aStream
		nextPutAll: name;
		nextPut: $ .
	self printVersionRevisionOn: aStream
]

{ #category : 'printing' }
Feature >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPut: $(.
	self printDetailsOn: aStream.
	aStream
		nextPut: $)
]

{ #category : 'printing' }
Feature >> printVersionRevisionOn: aStream [
	version printOn: aStream.
	aStream nextPut: $..
	revision printOn: aStream
]

{ #category : 'accessing' }
Feature >> requirementOfMe [

	^ FeatureRequirement 
		name: self name 
		minVersion: self version 
		minRevision: self revision 
		maxVersion: nil
]

{ #category : 'accessing' }
Feature >> revision [

	^ revision
]

{ #category : 'testing' }
Feature >> satisfies: featureRequirement [
	"Does this provided Feature satisfy the FeatureRequirement?"
	
	"Must match name."
	^ (name sameAs: featureRequirement name) and: [

		"If no specific version req, we are done. Ok."
		featureRequirement minVersion isNil or: [

		"If our version is exactly the min req version, we must also satisfy minRevision"
		version = featureRequirement minVersion and: [
			featureRequirement minRevision isNil or: [ revision >= featureRequirement minRevision ]]] or: [
		
		"If we are past min req version, ignore minRevision, but check we are not beyond max req version"
		version > featureRequirement minVersion and: [
			featureRequirement maxVersion isNil or: [ version <= featureRequirement maxVersion ]]]]
]

{ #category : 'accessing' }
Feature >> version [

	^ version

]

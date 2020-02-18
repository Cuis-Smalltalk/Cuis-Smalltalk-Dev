'From Cuis 5.0 [latest update: #4034] on 17 February 2020 at 3:33:24 pm'!

!Feature class methodsFor: 'convenience' stamp: 'jmv 2/17/2020 15:27:42'!
require: featureName version: integerVersion revision: integerRevision
	"
	Feature require: 'Sound' version: 1 revision: 0
	Feature require: 'Tests' version: 1 revision: 0
	"
	(FeatureRequirement name: featureName version: integerVersion revision: integerRevision) require! !


!Feature class methodsFor: 'convenience' stamp: 'jmv 2/17/2020 15:27:34'!
require: featureName version: versionNumber
	"
	Feature require: 'StyledTextInstaller' version: 1
	Feature require: 'Sound' version: 1
	Feature require: 'Tests' version: 1
	"
	(FeatureRequirement name: featureName version: versionNumber) require! !


!FeatureRequirement methodsFor: 'printing' stamp: 'jmv 2/17/2020 15:26:20'!
printDetailsOn: aStream
	aStream
		nextPutAll: name;
		nextPut: $ .
	minVersion
		ifNil: [
			aStream nextPutAll: '*.*)'.
			^self ]
		ifNotNil: [ minVersion printOn: aStream ].
	aStream nextPut: $..
	minRevision
		ifNil: [ aStream nextPut: $* ]
		ifNotNil: [ minRevision printOn: aStream ].
	(minRevision notNil or: [ maxVersion isNil or: [maxVersion > minVersion ]]) ifTrue: [
		aStream nextPutAll: ' to '.
		maxVersion
			ifNil: [ aStream nextPut: $* ]
			ifNotNil: [ maxVersion printOn: aStream ].
		maxVersion = minVersion
			ifTrue: [ aStream nextPutAll: '.999' ]
			ifFalse: [ aStream nextPutAll: '.*' ]
		]! !


!FeatureRequirement class methodsFor: 'instance creation' stamp: 'jmv 2/17/2020 15:32:03'!
name: aSymbol minVersion: minVersionOrNil minRevision: minRevisionOrNil maxVersion: maxVersionOrNil

	| newInst |
	(minVersionOrNil isNil or: [ minVersionOrNil isInteger ]) ifFalse: [
		self error: 'Version numbers must be Integer numbers. Specify also Revision number if needed.' ].
	(minRevisionOrNil isNil or: [ minRevisionOrNil isInteger ]) ifFalse: [
		self error: 'Revision numbers must be Integer numbers.' ].
	(maxVersionOrNil isNil or: [ maxVersionOrNil isInteger ]) ifFalse: [
		self error: 'Version numbers must be Integer numbers. Specify also Revision number if needed.' ].
	newInst _ self new.
	newInst name: aSymbol minVersion: minVersionOrNil minRevision: minRevisionOrNil maxVersion: maxVersionOrNil.
	
	^ newInst! !


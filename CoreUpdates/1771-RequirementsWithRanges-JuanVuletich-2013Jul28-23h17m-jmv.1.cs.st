'From Cuis 4.2 of 25 July 2013 [latest update: #1769] on 29 July 2013 at 12:08:52.311477 am'!
!classDefinition: #FeatureRequirement category: #'Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name version revision pathName minRevision maxVersion minVersion '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!FeatureRequirement commentStamp: '<historical>' prior: 0!
A package has a (possibly empty) set of FeatureRequirement's which specifies acceptable version range and minimum revision.  See class FeatureSpec.

  name -- a symbol naming the feature
  minVersion -- the minimum version required
  maxVersion -- the maximum version required
  minRevision -- the minumum revision of the minVersion (assumed zero if minVersion does not equal maxVersion
  path -- nil or the (cached)  pathName of a file containing the package with this feature


Try any of:
  (FeatureRequirement name: #'Sound') require.		"Ask for whatever version of Sound is available"
  (FeatureRequirement name: #'Sound' version: 1) require. 		"Ask for a specific version. Any revision will do"
  (FeatureRequirement name: #'Sound' version: 1 revision: 0) require.  	"Ask for a specific version.revision"
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: 3) require 		"Specific version.revision, or any greater version, up to max"

This should load packages #Compression and #Sound


  (FeatureRequirement name: #'Sound') print
  (FeatureRequirement name: #'Sound' version: 1) print
  (FeatureRequirement name: #'Sound' version: 1 revision: 12) print
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: nil) print
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: nil maxVersion: 3) print
  (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: 3) print!


!Feature methodsFor: 'incrementing' stamp: 'jmv 7/28/2013 23:32'!
newRevision
	"Answer a new version of me"

	^Feature name: name version: version revision: revision + 1! !

!Feature methodsFor: 'incrementing' stamp: 'jmv 7/28/2013 23:32'!
newVersion
	"Answer a new version of me"

	^Feature name: name version: version + 1 revision: 0! !

!FeatureRequirement methodsFor: 'comparing' stamp: 'jmv 7/29/2013 00:37'!
= another
	^name = another name and: [
		minVersion = another minVersion and: [
		minRevision = another minRevision and: [
		maxVersion = another maxVersion ]]]! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'KenD 7/7/2013 14:11'!
maxVersion 

	^maxVersion ! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'KenD 7/7/2013 14:11'!
minRevision

	^minRevision ! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'KenD 7/7/2013 14:11'!
minVersion 

	^minVersion ! !

!FeatureRequirement methodsFor: 'initialize-release' stamp: 'jmv 7/28/2013 23:54'!
name: featureName minVersion: minVer minRevision: rev maxVersion: maxVer

	name := featureName asSymbol.
	minVersion := minVer.
	minRevision := rev.
	maxVersion := maxVer! !


!FeatureRequirement class methodsFor: 'instance creation' stamp: 'jmv 7/28/2013 23:54'!
name: aSymbol minVersion: minVersionOrNil minRevision: minRevisionOrNil maxVersion: maxVersionOrNil

	| newInst |
	newInst _ self new.
	newInst name: aSymbol minVersion: minVersionOrNil minRevision: minRevisionOrNil maxVersion: maxVersionOrNil.
	
	^ newInst! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 7/29/2013 00:07'!
scanSpecificChangeRecordType
	"Scan anything that involves more than one chunk"

	| itemPosition item item2 tokens firstToken secondToken stamp anIndex def isMeta name record methodReference doItOnlyIfInBaseSystem feature |
	itemPosition _ file position.
	item _ file nextChunk.
	(((item includesSubString: 'commentStamp:')
	or: [item includesSubString: 'methodsFor:']
	or: [item includesSubString: 'classDefinition:']
	or: [item includesSubString: 'classRemoval:']
	or: [item includesSubString: 'methodRemoval:'])
	or: [item includesSubString: 'methodMoveToSomePackage:']
	or: [item includesSubString: 'classMoveToSomePackage:']
	or: [item includesSubString: 'provides']
	or: [item includesSubString: 'requires'])
		ifFalse: [
			"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
			^ self addItem: (ChangeRecord new file: file position: itemPosition type: #preamble)
				 text: ('preamble: ' , item contractTo: 160)].

	tokens _ Smalltalk actualScannerClass new scanTokens: item.
	tokens size >= 2 ifTrue: [
		stamp _ ''.
		anIndex _ tokens indexOf: #stamp: ifAbsent: nil.
		anIndex ifNotNil: [stamp _ tokens at: (anIndex + 1)].
		firstToken _ tokens first.
		secondToken _ tokens second.

		firstToken == #classDefinition: ifTrue: [
			itemPosition _ file position.
			isMeta _ secondToken includesSubString: ' class'.
			name _ isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
			def _ file nextChunk.
			record _ ChangeRecord new file: file position: itemPosition type: #classDefinition
				class: name asSymbol category: tokens last meta: isMeta stamp: nil.
			self addItem: record text: 'classDefinition: ', def.
			^ self ].

		(firstToken == #classRemoval: or: [ firstToken == #classMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #classMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				self 
					addItem: (ClassDeletionChangeRecord new
						clsName: secondToken;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['clase move to some package: '] ifFalse: ['class removal: ']), secondToken ].
			^ self ].

		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #methodMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				isMeta _ tokens third == #class.
				isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
				methodReference _ (MethodReference new
					setClassSymbol: secondToken
					classIsMeta: isMeta
					methodSymbol: tokens last
					stringVersion: secondToken, ' ', (isMeta ifTrue: ['class '] ifFalse: ['']), tokens last).
				self
					addItem: (MethodDeletionChangeRecord new
						methodReference: methodReference;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['method move to some package: '] ifFalse: ['method removal: ']), 
							methodReference asStringOrText ].
			^ self ].
		
		secondToken == #methodsFor: ifTrue: [
			^ self scanCategory: tokens third class: firstToken meta: false stamp: stamp].

		tokens third == #methodsFor: ifTrue: [
			^ self scanCategory: tokens fourth class: firstToken meta: true stamp: stamp].

		secondToken == #commentStamp: ifTrue: [
			stamp _ tokens third.
			self addItem:
					(ChangeRecord new file: file position: file position type: #classComment
									class: firstToken category: nil meta: false stamp: stamp)
					text: 'class comment for ' , firstToken, 
						  (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).
			file nextChunk.
			^ self ].

		firstToken == #provides: ifTrue: [
			feature _ FeatureChangeRecord new
					type: #provides
					feature: (Feature name: secondToken version: tokens third revision: tokens fourth).
			self
				addItem: feature
				text: feature string ].

		firstToken == #requires: ifTrue: [
			feature _ FeatureChangeRecord new
					type: #requires
					feature: (FeatureRequirement name: secondToken minVersion: tokens third minRevision: tokens fourth maxVersion: (tokens size > 4 ifTrue: [tokens fifth])).
			self
				addItem: feature
				text: feature string ].
	]! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 7/29/2013 00:06'!
writeFeatureSpecOn: aStream
	| provides |
	provides _ featureSpec provides.
	aStream
		nextPut: $!!; 
		nextChunkPut: 'provides: ', provides name asString printString, ' ', provides version printString, ' ', provides revision printString;
		newLine.
	featureSpec requires do: [ :requires |
		aStream
		nextPut: $!!; 
			nextChunkPut: 'requires: ', requires name asString printString, ' ', requires minVersion printString, ' ', requires minRevision printString, ' ', requires maxVersion printString;
			newLine ]! !


!Feature methodsFor: 'testing' stamp: 'jmv 7/28/2013 23:43'!
satisfies: featureRequirement
	"Does this provided Feature satisfy the FeatureRequirement?"
	
	"Must match name."
	^ name = featureRequirement name and: [

		"If no specific version req, we are done. Ok."
		featureRequirement minVersion isNil or: [

		"If our version is exactly the min req version, we must also satisfy minRevision"
		version = featureRequirement minVersion and: [
			featureRequirement minRevision isNil or: [ revision >= featureRequirement minRevision ]]] or: [
		
		"If we are past min req version, ignore minRevision, but check we are not beyond max req version"
		version > featureRequirement minVersion and: [
			featureRequirement maxVersion isNil or: [ version <= featureRequirement maxVersion ]]]]! !


!FeatureRequirement methodsFor: 'printing' stamp: 'jmv 7/29/2013 00:05'!
printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPut: $(;
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
		aStream nextPutAll: '.*'].
	aStream
		nextPut: $)! !


!FeatureRequirement class methodsFor: 'instance creation' stamp: 'jmv 7/28/2013 23:54'!
name: aSymbol
	"Ask for any version, and any revision"
	^self name: aSymbol minVersion: nil minRevision: nil maxVersion: nil! !

!FeatureRequirement class methodsFor: 'instance creation' stamp: 'jmv 7/28/2013 23:54'!
name: aSymbol version: integerVersion
	"Ask for a specific version, but any revision"
	^self name: aSymbol minVersion: integerVersion minRevision: nil maxVersion: integerVersion! !

!FeatureRequirement class methodsFor: 'instance creation' stamp: 'jmv 7/28/2013 23:55'!
name: aSymbol version: integerVersion revision: integerRevision
	"Ask for a specific version, and revision"
	^self name: aSymbol minVersion: integerVersion minRevision: integerRevision maxVersion: integerVersion! !

!methodRemoval: FeatureRequirement class #name:minVersion:maxVersion:minRevision:!
FeatureRequirement class removeSelector: #name:minVersion:maxVersion:minRevision:!
!methodRemoval: FeatureRequirement #name:minVersion:maxVersion:minRevision:!
FeatureRequirement removeSelector: #name:minVersion:maxVersion:minRevision:!
!methodRemoval: FeatureRequirement #name:version:revision:!
FeatureRequirement removeSelector: #name:version:revision:!
!methodRemoval: FeatureRequirement #revision!
FeatureRequirement removeSelector: #revision!
!methodRemoval: FeatureRequirement #version!
FeatureRequirement removeSelector: #version!
!classDefinition: #FeatureRequirement category: #'Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minVersion minRevision maxVersion pathName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!Feature reorganize!
('accessing' name revision version)
('requires')
('testing' satisfies:)
('initialize-release' name:version:revision:)
('printing' printOn:)
('private')
('incrementing' newRevision newVersion)
!


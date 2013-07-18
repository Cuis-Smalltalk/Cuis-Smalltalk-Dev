'From Cuis 4.1 of 12 December 2012 [latest update: #1750] on 9 July 2013 at 11:46:26.772291 pm'!
!classDefinition: #FeatureChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #FeatureChangeRecord
	instanceVariableNames: 'type feature'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
!classDefinition: #CodePackageFile category: #'Package Support'!
CodeFile subclass: #CodePackageFile
	instanceVariableNames: 'packageName description classesToRemove methodsToRemove featureSpec '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

!FeatureChangeRecord methodsFor: 'accessing' stamp: 'jmv 7/7/2013 22:29'!
changeType
	^ type! !

!FeatureChangeRecord methodsFor: 'accessing' stamp: 'jmv 7/7/2013 22:54'!
feature
	^feature! !

!FeatureChangeRecord methodsFor: 'accessing' stamp: 'jmv 7/7/2013 22:29'!
methodSelector
	^nil! !

!FeatureChangeRecord methodsFor: 'accessing' stamp: 'jmv 7/7/2013 22:31'!
string
	^ type, ': ', feature printString! !

!FeatureChangeRecord methodsFor: 'accessing' stamp: 'jmv 7/7/2013 22:25'!
type: aSymbol feature: aFeature
	type _ aSymbol.
	feature _ aFeature! !


!CodePackageFile methodsFor: 'accessing' stamp: 'jmv 7/7/2013 22:53'!
summary
	^featureSpec printString, String newLineString, super summary! !

!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 7/7/2013 22:54'!
provides: aFeatureChangeRecord
	featureSpec provides: aFeatureChangeRecord feature! !

!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 7/7/2013 22:54'!
requires: aFeatureChangeRecord
	featureSpec requires: aFeatureChangeRecord feature! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 7/7/2013 22:34'!
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
					feature: (Feature name: secondToken version: tokens third revision: tokens fourth).
			self
				addItem: feature
				text: feature string ].
	]! !


!CodePackageFile methodsFor: 'initialize' stamp: 'jmv 7/7/2013 22:36'!
initialize
	super initialize.
	description _ ''.
	featureSpec _ FeatureSpec new! !

!classDefinition: #CodePackageFile category: #'Package Support'!
CodeFile subclass: #CodePackageFile
	instanceVariableNames: 'packageName description classesToRemove methodsToRemove featureSpec'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Package Support'!

'From Cuis 4.1 of 12 December 2012 [latest update: #1745] on 7 July 2013 at 11:06:16.429838 pm'!
!ChangeList methodsFor: 'scanning' stamp: 'jmv 7/7/2013 19:01'!
scanSpecificChangeRecordType
	"Scan anything that involves more than one chunk"

	| itemPosition item item2 tokens firstToken secondToken stamp anIndex def isMeta name record methodReference doItOnlyIfInBaseSystem |
	itemPosition _ file position.
	item _ file nextChunk.
	(((item includesSubString: 'commentStamp:')
	or: [item includesSubString: 'methodsFor:']
	or: [item includesSubString: 'classDefinition:']
	or: [item includesSubString: 'classRemoval:']
	or: [item includesSubString: 'methodRemoval:'])
	or: [item includesSubString: 'methodMoveToSomePackage:']
	or: [item includesSubString: 'classMoveToSomePackage:'])
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
			^ self ]]! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 7/7/2013 18:48'!
scanFile: aFile from: startPosition to: stopPosition

	file _ aFile.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	file position: startPosition.
	'Scanning ', aFile localName, '...'
		displayProgressAt: Sensor mousePoint
		from: startPosition to: stopPosition
		during: [ :bar |
			[file position < stopPosition] whileTrue: [ | prevChar |
				bar value: file position.
				[file atEnd not and: [file peek isSeparator]]
					whileTrue: [prevChar _ file next].
				(file peekFor: $!!)
					ifTrue: [
						"A line starting with $!! means a specific ChangeRecord type"
						(prevChar notNil and: [ prevChar isLineSeparator ])
							ifTrue: [self scanSpecificChangeRecordType]]
					ifFalse: [
						"Otherwise, interpret it with #doIt:"
						| itemPosition item |
						itemPosition _ file position.
						item _ file nextChunk.
						item size > 0 ifTrue: [
							self
								addItem: (ChangeRecord new file: file position: itemPosition type: #doIt)
								text: 'do it: ' , (item contractTo: 160)]]]].
	self clearSelections! !


!CodeFile methodsFor: 'change record types' stamp: 'jmv 7/7/2013 18:57'!
classComment: chgRec
	"See senders of #classComment "
	(self getClass: chgRec changeClassName) classComment: chgRec! !

!CodeFile methodsFor: 'change record types' stamp: 'jmv 7/7/2013 18:57'!
classDefinition: chgRec
	"See senders of #classDefinition "
	| string |
	string := chgRec string.
	^chgRec isMetaClassChange 
		ifTrue: [self metaClassDefinition: string with: chgRec]
		ifFalse: [self classDefinition: string with: chgRec]! !

!CodeFile methodsFor: 'change record types' stamp: 'jmv 7/7/2013 18:58'!
doIt: chgRec
	"See senders of #doIt "
	| string |
	string := chgRec string.

	"Just for compatibility with Squeak, as Cuis always adds the #classDefinition change type marker in the files."
	('*ubclass:*instanceVariableNames:*classVariableNames:*poolDictionaries:*category:*'
		match: string) ifTrue:[^self classDefinition: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #classDefinition change type marker in the files."
	('* class*instanceVariableNames:*'
		match: string) ifTrue:[^self metaClassDefinition: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #methodRemoval: (or similar) change type marker in the files."
	('* removeSelector: *'
		match: string) ifTrue:[^self removedMethod: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #classComment change type marker in the files."
	('* comment:*'
		match: string) ifTrue:[^self msgClassComment: string with: chgRec].

	"Don't add these to a CodeFile. They will be added on save if needed."
	('* initialize'
		match: string) ifTrue:[^self]. "Initialization is done based on class>>initialize"

	('''From *'
		match: string) ifTrue:[^self possibleSystemSource: chgRec].
	doIts add: chgRec.! !

!CodeFile methodsFor: 'change record types' stamp: 'jmv 7/7/2013 18:58'!
method: chgRec
	"See senders of #method "
	(self getClass: chgRec changeClassName) methodChange: chgRec! !

!CodeFile methodsFor: 'change record types' stamp: 'jmv 7/7/2013 18:58'!
preamble: chgRec
	"See senders of #preamble "
	self doIt: chgRec! !

!methodRemoval: ChangeList #scanCategory!
ChangeList removeSelector: #scanCategory!

'From Cuis 4.1 of 12 December 2012 [latest update: #1614] on 24 February 2013 at 12:03:20 am'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 2/23/2013 22:07'!
annotationForSelector: aSelector ofClass: aClass 
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"
	| stamp sendersCount implementorsCount aCategory separator aString aList aComment stream requestList |
	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].
	aSelector == #Hierarchy
		ifTrue: [^ self annotationForHierarchyFor: aClass].
	stream _ WriteStream on: String new.
	requestList _ self annotationRequests.
	separator _ requestList size > 1
				ifTrue: [self annotationSeparator]
				ifFalse: [''].
	requestList
		do: [:aRequest | 
			aRequest == #firstComment
				ifTrue: [
					aComment _ aClass firstCommentAt: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream nextPutAll: aComment , separator]].
			aRequest == #masterComment
				ifTrue: [
					aComment _ aClass supermostPrecodeCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream nextPutAll: aComment , separator]].
			aRequest == #documentation
				ifTrue: [
					aComment _ aClass precodeCommentOrInheritedCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream nextPutAll: aComment , separator]].
			aRequest == #timeStamp
				ifTrue: [
					stamp _ self timeStamp.
					stream
						nextPutAll: (stamp size > 0
								ifTrue: [stamp , separator]
								ifFalse: ['no timeStamp' , separator])].
			aRequest == #messageCategory
				ifTrue: [
					aCategory _ aClass organization categoryOfElement: aSelector.
					aCategory
						ifNotNil: ["woud be nil for a method no longer present,  
							e.g. in a recent-submissions browser"
							stream nextPutAll: aCategory , separator]].
			aRequest == #sendersCount
				ifTrue: [
					sendersCount _ Smalltalk numberOfSendersOf: aSelector.
					sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
					stream nextPutAll: sendersCount , separator].
			aRequest == #implementorsCount
				ifTrue: [
					implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
					implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
					stream nextPutAll: implementorsCount , separator].
			aRequest == #priorVersionsCount
				ifTrue: [
					self
						addPriorVersionsCountForSelector: aSelector
						ofClass: aClass
						to: stream].
			aRequest == #priorTimeStamp
				ifTrue: [
					stamp _ VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
					stamp
						ifNotNil: [stream nextPutAll: 'prior time stamp: ' , stamp , separator]].
			aRequest == #recentChangeSet
				ifTrue: [
					aString _ ChangeSorter mostRecentChangeSetWithChangeForClass: aClass selector: aSelector.
					aString size > 0
						ifTrue: [stream nextPutAll: aString , separator]].
			aRequest == #allChangeSets
				ifTrue: [
					aList _ ChangeSorter allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set ']
								ifFalse: [stream nextPutAll: 'in change sets: '].
							aList
								do: [:aChangeSet | stream nextPutAll: aChangeSet name , ' ']]
						ifFalse: [stream nextPutAll: 'in no change set'].
					stream nextPutAll: separator].
			aRequest == #allBaseSystemChangeSets
				ifTrue: [
					aList _ (ChangeSorter allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it codePackage isNil ].
					aList size > 0
						ifTrue: [
							aList size = 1
								ifTrue: [stream nextPutAll: 'only in base system change set']
								ifFalse: [stream nextPutAll: 'in base system change sets:'].
							aList do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]]
						ifFalse: [stream nextPutAll: 'in no base system change set'].
					stream nextPutAll: separator].
			aRequest == #closuresInfo
				ifTrue: [
					aString _ aClass closuresInfoAt: aSelector.
					aString size > 0
						ifTrue: [stream nextPutAll: aString , separator]].
			aRequest == #packages
				ifTrue: [
					(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
						(CodePackage packageOfMethod: cm methodReference ifNone: nil)
							ifNil: [ stream nextPutAll: 'part of base system (i.e. not in a package)' ]
							ifNotNil: [ :codePackage |
								stream nextPutAll: 'in package '; nextPutAll: codePackage name ].
						stream nextPutAll: separator]].
		].
	^ stream contents! !


!IntegerTest methodsFor: 'tests - printing' stamp: 'jmv 2/24/2013 00:00'!
testPrintOnBaseShowRadix
	| s |
	s _ WriteStream on: String new.
	123 printOn: s base: 10 showRadix: false.
	self assert: (s contents = '123').

	s _ WriteStream on: String new.
	123 printOn: s base: 10 showRadix: true.
	self assert: (s contents = '10r123').

	s _ WriteStream on: String new.
	123 printOn: s base: 8 showRadix: false.
	self assert: (s contents = '173').

	s _ WriteStream on: String new.
	123 printOn: s base: 8 showRadix: true.
	self assert: (s contents = '8r173')! !


!NetNameResolver class methodsFor: 'address string utils' stamp: 'jmv 2/23/2013 22:08'!
stringFromAddress: addr
	"Return a string representing the given host address as four decimal bytes delimited with decimal points."
	"NetNameResolver stringFromAddress: NetNameResolver localHostAddress"

	| s |
	s _ WriteStream on: String new.
	1 to: 3 do: [ :i | (addr at: i) printOn: s. s nextPut: $. ].
	(addr at: 4) printOn: s.
	^ s contents! !


!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 2/23/2013 22:08'!
createPageTestWorkspace
	"Used to generate a workspace window for testing page up and page down stuff."
	"Utilities createPageTestWorkspace"

	| numberOfLines maxStringLength minLineCounterSize lineCounterSize offsetSize stream headerConstant prevStart prevStrLen prevLineNumber stringLen lineNumber start log pad charIndex char |
	numberOfLines _ 400.
	maxStringLength _ 22.
	minLineCounterSize _ 3.
	lineCounterSize _ (numberOfLines log asInteger + 1) max: minLineCounterSize.
	offsetSize _ 5.
	stream _ WriteStream on: String new.
	headerConstant _ lineCounterSize + 1 + offsetSize + 1.
	prevStart _ headerConstant negated.
	prevStrLen _ 0.
	prevLineNumber _ 0.
	numberOfLines timesRepeat: [
		stringLen _ maxStringLength atRandom max: 1.
		lineNumber _ prevLineNumber + 1.
		start _ prevStart + prevStrLen + headerConstant + 1.
		prevStart _ start.
		prevStrLen _ stringLen.
		prevLineNumber _ lineNumber.
		log _ lineNumber log asInteger.
		pad _ lineCounterSize - log - 1.
		pad timesRepeat: [stream nextPutAll: '0'].
		stream nextPutAll: lineNumber printString.
		stream space.
		log _ start log asInteger.
		pad _ offsetSize - log - 1.
		pad timesRepeat: [stream nextPutAll: '0'].
		stream nextPutAll: start printString.
		stream space.
		charIndex _ 'a' first asInteger.
		stringLen timesRepeat: [
			char _ Character value: charIndex.
			charIndex _ charIndex + 1.
			stream nextPut: char].
		lineNumber = numberOfLines ifFalse: [stream newLine]
		].
	(Workspace new contents: stream contents) openLabel: 'Test Data'.
! !

!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 2/23/2013 22:09'!
instanceComparisonsBetween: fileName1 and: fileName2
	"For differential results, run printSpaceAnalysis twice with different fileNames,
	then run this method...
		Smalltalk printSpaceAnalysis: 0 on: 'STspace.text1'.
			--- do something that uses space here ---
		Smalltalk printSpaceAnalysis: 0 on: 'STspace.text2'.
		Smalltalk instanceComparisonsBetween: 'STspace.text1' and 'STspace.text2'"

	| instCountDict report f aString items className newInstCount oldInstCount newSpace oldPair oldSpace |
	instCountDict _ Dictionary new.
	report _ WriteStream on: String new.
	f _ FileStream readOnlyFileNamed: fileName1.
	[f atEnd] whileFalse: [
		aString _ f crLfNextLine.
		items _ aString findTokens: ' '.
		(items size = 4 or: [items size = 5]) ifTrue:
			[instCountDict at: items first put: (Array with: items third asNumber with: items fourth asNumber)]].
	f close.

	f _ FileStream readOnlyFileNamed: fileName2.
	[f atEnd] whileFalse: [
		aString _ f crLfNextLine.
		items _ aString findTokens: ' '.
		(items size = 4 or: [items size = 5]) ifTrue:
			[className _ items first.
			newInstCount _ items third asNumber.
			newSpace _ items fourth asNumber.
			oldPair _ instCountDict at: className ifAbsent: nil.
			oldInstCount _ oldPair ifNil: [0] ifNotNil: [oldPair first].
			oldSpace _ oldPair ifNil: [0] ifNotNil: [oldPair second].
			oldInstCount ~= newInstCount ifTrue:
				[report nextPutAll: (newInstCount - oldInstCount) printString; tab; nextPutAll: (newSpace - oldSpace) printString; tab; nextPutAll: className asString; newLine]]].
	f close.

	(TextModel new contents: report contents)
		openLabel: 'Instance count differentials between ', fileName1, ' and ', fileName2! !


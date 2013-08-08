'From Cuis 4.2 of 25 July 2013 [latest update: #1783] on 5 August 2013 at 10:51:46.525903 am'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 8/5/2013 10:49'!
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
			aRequest == #allChangeSets
				ifTrue: [
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
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
					aList _ (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it codePackage isNil ].
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
								stream nextPutAll: 'in package '; nextPutAll: codePackage packageName ].
						stream nextPutAll: separator]].
		].
	^ stream contents! !


!SinglePackageBrowser methodsFor: 'accessing' stamp: 'jmv 8/5/2013 10:50'!
defaultBrowserTitle
	^ 'Browser for package: ', package packageName! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 8/5/2013 10:48'!
changeSetForPackage: aCodePackage
	| csName |
	aCodePackage ifNil: [ ^ self changeSetForBaseSystem ].
	^self notInstallOrTestRun
		ifTrue: [
			ChangeSet existingOrNewChangeSetForPackage: aCodePackage ]
		ifFalse: [
			csName _ Installing = ('Install-' , aCodePackage packageName)
				ifTrue: [ Installing ]
				ifFalse: [ 'Affects-' , aCodePackage packageName, '--' , Installing ].
			ChangeSet
				existingOrNewChangeSetNamed: csName
				forBaseSystem: false ]! !

!ChangeSet class methodsFor: 'enumerating' stamp: 'jmv 8/5/2013 10:48'!
existingOrNewChangeSetForPackage: aCodePackage
	| answer |
	answer _ ChangeSet
		existingOrNewChangeSetNamed: 'UnsavedChangesTo-' , aCodePackage packageName
		forBaseSystem: false.
	answer codePackage: aCodePackage.
	^answer! !

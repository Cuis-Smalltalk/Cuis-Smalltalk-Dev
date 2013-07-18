'From Cuis 4.1 of 12 December 2012 [latest update: #1525] on 30 December 2012 at 11:06:59 pm'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 12/30/2012 23:06'!
annotationForSelector: aSelector ofClass: aClass 
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"
	| stamp sendersCount implementorsCount aCategory separator aString aList aComment aStream requestList |
	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].
	aSelector == #Hierarchy
		ifTrue: [^ self annotationForHierarchyFor: aClass].
	aStream _ ReadWriteStream on: ''.
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
						ifFalse: [aStream nextPutAll: aComment , separator]].
			aRequest == #masterComment
				ifTrue: [
					aComment _ aClass supermostPrecodeCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [aStream nextPutAll: aComment , separator]].
			aRequest == #documentation
				ifTrue: [
					aComment _ aClass precodeCommentOrInheritedCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [aStream nextPutAll: aComment , separator]].
			aRequest == #timeStamp
				ifTrue: [
					stamp _ self timeStamp.
					aStream
						nextPutAll: (stamp size > 0
								ifTrue: [stamp , separator]
								ifFalse: ['no timeStamp' , separator])].
			aRequest == #messageCategory
				ifTrue: [
					aCategory _ aClass organization categoryOfElement: aSelector.
					aCategory
						ifNotNil: ["woud be nil for a method no longer present,  
							e.g. in a recent-submissions browser"
							aStream nextPutAll: aCategory , separator]].
			aRequest == #sendersCount
				ifTrue: [
					sendersCount _ Smalltalk numberOfSendersOf: aSelector.
					sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
					aStream nextPutAll: sendersCount , separator].
			aRequest == #implementorsCount
				ifTrue: [
					implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
					implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
					aStream nextPutAll: implementorsCount , separator].
			aRequest == #priorVersionsCount
				ifTrue: [
					self
						addPriorVersionsCountForSelector: aSelector
						ofClass: aClass
						to: aStream].
			aRequest == #priorTimeStamp
				ifTrue: [
					stamp _ VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
					stamp
						ifNotNil: [aStream nextPutAll: 'prior time stamp: ' , stamp , separator]].
			aRequest == #recentChangeSet
				ifTrue: [
					aString _ ChangeSorter mostRecentChangeSetWithChangeForClass: aClass selector: aSelector.
					aString size > 0
						ifTrue: [aStream nextPutAll: aString , separator]].
			aRequest == #allChangeSets
				ifTrue: [
					aList _ ChangeSorter allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [aStream nextPutAll: 'only in change set ']
								ifFalse: [aStream nextPutAll: 'in change sets: '].
							aList
								do: [:aChangeSet | aStream nextPutAll: aChangeSet name , ' ']]
						ifFalse: [aStream nextPutAll: 'in no change set'].
					aStream nextPutAll: separator].
			aRequest == #allBaseSystemChangeSets
				ifTrue: [
					aList _ (ChangeSorter allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it codePackage isNil ].
					aList size > 0
						ifTrue: [
							aList size = 1
								ifTrue: [aStream nextPutAll: 'only in base system change set']
								ifFalse: [aStream nextPutAll: 'in base system change sets:'].
							aList do: [:aChangeSet | aStream nextPut: Character space; nextPutAll: aChangeSet name ]]
						ifFalse: [aStream nextPutAll: 'in no base system change set'].
					aStream nextPutAll: separator].
			aRequest == #closuresInfo
				ifTrue: [
					aString _ aClass closuresInfoAt: aSelector.
					aString size > 0
						ifTrue: [aStream nextPutAll: aString , separator]].
			aRequest == #packages
				ifTrue: [
					(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
						(CodePackage packageOfMethod: cm methodReference ifNone: nil)
							ifNil: [ aStream nextPutAll: 'part of base system (i.e. not in a package)' ]
							ifNotNil: [ :codePackage |
								aStream nextPutAll: 'in package '; nextPutAll: codePackage name ].
						aStream nextPutAll: separator]].
		].
	^ aStream contents! !


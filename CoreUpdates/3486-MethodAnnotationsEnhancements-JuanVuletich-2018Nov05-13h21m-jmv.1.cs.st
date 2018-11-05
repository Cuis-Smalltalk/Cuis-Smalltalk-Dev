'From Cuis 5.0 of 7 November 2016 [latest update: #3485] on 5 November 2018 at 1:32:47 pm'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 11/5/2018 13:32:18'!
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
			aRequest == #packages
				ifTrue: [
					(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
						(CodePackage packageOfMethod: cm methodReference ifNone: nil)
							ifNil: [ stream nextPutAll: 'part of base system (i.e. not in a package)' ]
							ifNotNil: [ :codePackage |
								stream nextPutAll: 'in package '; nextPutAll: codePackage packageName ].
						stream nextPutAll: separator]].
			aRequest == #changeSets
				ifTrue: [
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set ']
								ifFalse: [stream nextPutAll: 'in change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no change set'].
					stream nextPutAll: separator].
			aRequest == #allChangeSets
				ifTrue: [
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set ']
								ifFalse: [stream nextPutAll: 'in change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no change set'].
					stream nextPutAll: separator].
			aRequest == #allBaseSystemChangeSets
				ifTrue: [
					aList _ (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
					aList size > 0
						ifTrue: [
							aList size = 1
								ifTrue: [stream nextPutAll: 'only in base system change set']
								ifFalse: [stream nextPutAll: 'in base system change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no base system change set'].
					stream nextPutAll: separator].
			aRequest == #closuresInfo
				ifTrue: [
					aString _ aClass closuresInfoAt: aSelector.
					aString size > 0
						ifTrue: [stream nextPutAll: aString , separator]].
		].
	^ stream contents! !


!Preferences class methodsFor: 'parameters' stamp: 'jmv 11/5/2018 13:23:10'!
setDefaultAnnotationInfo
	"
	Preferences setDefaultAnnotationInfo
	"
	^ self parameters at: #MethodAnnotations put: #(timeStamp messageCategory implementorsCount sendersCount  packages changeSets)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

Preferences setDefaultAnnotationInfo!


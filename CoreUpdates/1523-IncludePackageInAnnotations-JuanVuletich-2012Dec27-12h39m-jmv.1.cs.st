'From Cuis 4.1 of 12 December 2012 [latest update: #1522] on 27 December 2012 at 12:39:39 pm'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 12/27/2012 12:07'!
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
					aList _ (ChangeSorter allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
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
					(CodePackage packageOfMethod: ( aClass compiledMethodAt: aSelector) methodReference ifNone: nil)
						ifNil: [ aStream nextPutAll: 'part of base system (i.e. not in a package)' ]
						ifNotNil: [ :codePackage |
							aStream nextPutAll: 'in package '; nextPutAll: codePackage name ].
					aStream nextPutAll: separator].
		].
	^ aStream contents! !


!ChangeList methodsFor: 'viewing access' stamp: 'jmv 12/27/2012 11:52'!
annotation
	"Answer the string to be shown in an annotation pane.  Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact."

	| change count selectedCount ann1 ann2 aClass |
	change _ self currentChange.
	
	change isNil ifTrue: [
		count _ listSelections size.
		selectedCount _ listSelections count: [ :flag | flag ].
		^ 'Total items: ', count printString, ' - Selected items: ', selectedCount printString ].

	change type == #classDefinition ifTrue: [
		ann1 _ change isMetaClassChange ifTrue: [ 'Metaclass' ] ifFalse: [ 'Class' ].
		ann2 _ (Smalltalk includesKey: change methodClassName) ifTrue: [ ' already exists' ] ifFalse: [ ' not in system' ].
		^ann1, ann2 ].
	
	(self selectedMessageName isNil or: [self selectedClassOrMetaClass isNil])
		ifTrue: [^ '------'].

	^ change methodSelector notNil
		ifFalse: [ super annotation]
		ifTrue: [
			((aClass _ change methodClass) isNil or: [(aClass includesSelector: change methodSelector) not])
				ifTrue: [
					change methodClassName, ' >> ', change methodSelector, ' is not present in the system.']
				ifFalse: [
					'current version: ', super annotation]]! !


!Preferences class methodsFor: 'parameters' stamp: 'jmv 12/26/2012 13:40'!
annotationInfo 
	"Answer a list of pairs characterizing all the available kinds of annotations; in each pair, the first element is a symbol representing the info type, and the second element is a string providing the corresponding balloon help"

	^ #(
		(timeStamp				'The time stamp of the last submission of the method.')
		(firstComment			'The first comment in the method, if any.')
		(masterComment		'The comment at the beginning of the supermost implementor of the method if any.')
		(documentation			'Comment at beginning of the method or, if it has none, comment at the beginning of a superclass''s implementation of the method')
		(messageCategory		'Which method category the method lies in')
		(sendersCount			'A report of how many senders there of the message.')
		(implementorsCount	'A report of how many implementors there are of the message.')
		(recentChangeSet		'The most recent change set bearing the method.')
		(allChangeSets			'A list of all change sets bearing the method.')
		(priorVersionsCount	'A report of how many previous versions there are of the method' )
		(priorTimeStamp			'The time stamp of the penultimate submission of the method, if any')
		(closuresInfo				'Details about BlockClosures in the method')
		(packages				'Details about CodePackages including the method')
	)! !

!Preferences class methodsFor: 'parameters' stamp: 'jmv 12/26/2012 13:44'!
setDefaultAnnotationInfo
	"
	Preferences setDefaultAnnotationInfo
	"
	^ Parameters at: #MethodAnnotations put: #(timeStamp messageCategory implementorsCount sendersCount closuresInfo allBaseSystemChangeSets packages)! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences setDefaultAnnotationInfo!


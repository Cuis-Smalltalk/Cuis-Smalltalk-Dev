'From Cuis7.5 [latest update: #7351] on 14 July 2025 at 4:59:03 pm'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 7/14/2025 16:57:14'!
annotationForSelector: aSelector ofClass: aClass
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"

	| method |
	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].

	method := aClass compiledMethodAt: aSelector ifAbsent: nil.
	^ String streamContents: [ :strm |
		(Preferences at: #methodAnnotations)
			do: [ :each |
				each caseOf: {
					[#firstComment] -> [
						strm nextPutAll: (aClass firstCommentAt: aSelector) ].
					[#masterComment] -> [
						strm nextPutAll: ((aClass supermostPrecodeCommentFor: aSelector) ifNil: ['']) ].
					[#documentation] -> [
						strm nextPutAll: ((aClass precodeCommentOrInheritedCommentFor: aSelector) ifNil: ['']) ].
					[#timeStamp] -> [ | stamp |
						stamp := self timeStamp.
						strm nextPutAll: (stamp size > 0 ifTrue: [stamp] ifFalse: ['no timestamp'])].
					[#linesOfCode] -> [
						method notNil ifTrue: [
							strm
								print: method linesOfCode;
								nextPutAll: ' lines of code' ]].
					[#messageSendsCount] -> [
						method notNil ifTrue: [
							strm
								print: method messageSendsCount;
								nextPutAll: ' message sends' ]].
					[#messageCategory] -> [
						strm nextPutAll: (( aClass organization categoryOfElement: aSelector) ifNil: ['']) ].
					[#sendersCount] -> [ | sendersCount |
						sendersCount := Smalltalk numberOfSendersOf: aSelector.
						sendersCount := sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
						strm nextPutAll: sendersCount ].
					[#implementorsCount] -> [ | implementorsCount |
						implementorsCount := Smalltalk numberOfImplementorsOf: aSelector.
						implementorsCount := implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
						strm nextPutAll: implementorsCount ].
					[#priorVersionsCount] -> [
						self addPriorVersionsCountForSelector: aSelector ofClass: aClass to: strm].
					[#priorTimeStamp] -> [ | stamp |
						stamp := VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
						strm nextPutAll: 'prior timestamp: '; nextPutAll: (stamp ifNil: ['None']) ].
					[#packages] -> [
						method notNil ifTrue: [
							(CodePackage packageOfMethod: method methodReference ifNone: nil)
								ifNil: [ strm nextPutAll: 'in no package' ]
								ifNotNil: [ :codePackage |
									strm nextPutAll: 'in package '; nextPutAll: codePackage packageName ]]].
					[#changeSets] -> [ | aList |
						aList := ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allChangeSets] -> [ | aList |
						aList := ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in change set']
									ifFalse: [strm nextPutAll: 'in change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no change set']].
					[#allBaseSystemChangeSets] -> [ | aList |
						aList := (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
						aList notEmpty
							ifTrue: [ aList size = 1
									ifTrue: [strm nextPutAll: 'only in base system change set']
									ifFalse: [strm nextPutAll: 'in base system change sets:'].
								aList
									do: [:aChangeSet | strm nextPut: Character space; nextPutAll: aChangeSet name ]
									separatedBy: [ strm nextPut: $, ]]
							ifFalse: [strm nextPutAll: 'in no base system change set']].
					
				}]
			separatedBy: [ strm nextPutAll: self annotationSeparator ].
		(method notNil and: [method hasBreakpoint]) ifTrue: [
			strm nextPutAll: self annotationSeparator.
			strm nextPutAll: '[Breakpoint]' ]
	].! !

!methodRemoval: PseudoClass #closuresInfoAt: stamp: 'jmv 7/14/2025 16:57:37'!
PseudoClass removeSelector: #closuresInfoAt:!
!methodRemoval: SystemDictionary #eliotsClosureMeasurements2 stamp: 'jmv 7/14/2025 16:56:02'!
SystemDictionary removeSelector: #eliotsClosureMeasurements2!
!methodRemoval: SystemDictionary #eliotsClosureMeasurements stamp: 'jmv 7/14/2025 16:55:31'!
SystemDictionary removeSelector: #eliotsClosureMeasurements!
!methodRemoval: SystemDictionary #browseMethodsWithOnlyCleanClosures stamp: 'jmv 7/14/2025 16:55:52'!
SystemDictionary removeSelector: #browseMethodsWithOnlyCleanClosures!
!methodRemoval: SystemDictionary #browseMethodsWithClosuresThatOnlyReadOuterTemps stamp: 'jmv 7/14/2025 16:55:43'!
SystemDictionary removeSelector: #browseMethodsWithClosuresThatOnlyReadOuterTemps!
!methodRemoval: SystemDictionary #browseMethodsWithClosuresThatAccessOuterTemps stamp: 'jmv 7/14/2025 16:55:40'!
SystemDictionary removeSelector: #browseMethodsWithClosuresThatAccessOuterTemps!
!methodRemoval: SystemDictionary #eliotsClosureMeasurements2On: stamp: 'jmv 7/14/2025 16:56:09'!
SystemDictionary removeSelector: #eliotsClosureMeasurements2On:!
!methodRemoval: SystemDictionary #closuresInfoStringForClass:selector: stamp: 'jmv 7/14/2025 16:58:02'!
SystemDictionary removeSelector: #closuresInfoStringForClass:selector:!
!methodRemoval: SystemDictionary #browseMethodsWithMoreThanOneClosure stamp: 'jmv 7/14/2025 16:55:49'!
SystemDictionary removeSelector: #browseMethodsWithMoreThanOneClosure!
!methodRemoval: SystemDictionary #eliotsClosureMeasurementsOn:over: stamp: 'jmv 7/14/2025 16:58:32'!
SystemDictionary removeSelector: #eliotsClosureMeasurementsOn:over:!
!methodRemoval: SystemDictionary #browseMethodsWithClosuresThatWriteOuterTemps stamp: 'jmv 7/14/2025 16:55:45'!
SystemDictionary removeSelector: #browseMethodsWithClosuresThatWriteOuterTemps!
!methodRemoval: SystemDictionary #browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise stamp: 'jmv 7/14/2025 16:55:46'!
SystemDictionary removeSelector: #browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise!
!methodRemoval: Behavior #closuresInfoAt: stamp: 'jmv 7/14/2025 16:57:38'!
Behavior removeSelector: #closuresInfoAt:!

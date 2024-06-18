'From Cuis7.1 [latest update: #6482] on 18 June 2024 at 4:50:37 pm'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 6/18/2024 16:39:40'!
numberOfDirectSendersOf: aSymbol
	"Answer the count of all the methods that call on aLiteral.
	[ (Smalltalk numberOfSendersOf: #open:label:) ] timeToRun
	[ (Smalltalk numberOfDirectSendersOf: #open:label:) ] timeToRun
	[ (Smalltalk numberOfSendersOf: #==) ] timeToRun
	[ (Smalltalk numberOfDirectSendersOf: #==) ] timeToRun
	"
	| count specialFlag specialByte |
	count := 0.
	specialFlag := self hasSpecialSelector: aSymbol ifTrueSetByte: [ :b | specialByte := b ].
	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :method |
			((method hasLiteral: aSymbol) or: [specialFlag and: [(method scanFor: specialByte)]])
				ifTrue: [ count := count + 1 ]]].
	^ count! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 6/18/2024 16:42:29'!
numberOfSendersOf: aSymbol
	"Answer the count of all the methods that call on aLiteral.
	[ (Smalltalk numberOfSendersOf: #open:label:) ] timeToRun
	[ (Smalltalk numberOfSendersOf: #==) ] timeToRun
	
	See #numberOfDirectSendersOf:
	"
	| count specialFlag specialByte |
	count := 0.
	specialFlag := self hasSpecialSelector: aSymbol ifTrueSetByte: [ :b | specialByte := b ].
	self allBehaviorsDo: [ :class |
		count := count + (class whichSelectorsReferTo: aSymbol special: specialFlag byte: specialByte) size ].
	^ count! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 6/18/2024 16:40:20'!
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
						sendersCount := Smalltalk numberOfDirectSendersOf: aSelector.
						sendersCount := sendersCount = 1
								ifTrue: ['1 direct sender']
								ifFalse: [sendersCount printString , ' direct senders'].
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
					[#closuresInfo] -> [
						strm nextPutAll: (aClass closuresInfoAt: aSelector)].
					
				}]
			separatedBy: [ strm nextPutAll: self annotationSeparator ].
		(method notNil and: [method hasBreakpoint]) ifTrue: [
			strm nextPutAll: self annotationSeparator.
			strm nextPutAll: '[Breakpoint]' ]
	].! !


!ChangeList methodsFor: 'viewing access' stamp: 'jmv 6/18/2024 16:40:39'!
annotation
	"Answer the string to be shown in an annotation pane.  Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact."

	| change count selectedCount ann1 ann2 |
	change := self currentChange.
	
	change isNil ifTrue: [
		count := listSelections size.
		selectedCount := listSelections count: [ :flag | flag ].
		^ 'Total items: ', count printString, ' - Selected items: ', selectedCount printString ].

	change changeType == #classDefinition ifTrue: [
		ann1 := change isMetaClassChange ifTrue: [ 'Metaclass' ] ifFalse: [ 'Class' ].
		ann2 := (Smalltalk includesKey: change changeClassName) ifTrue: [ ' already exists' ] ifFalse: [ ' not in system' ].
		^ann1, ann2 ].
	
	(self selectedMessageName isNil or: [self selectedClassOrMetaClass isNil])
		ifTrue: [^ ''].

	^ change methodSelector notNil
		ifFalse: [ super annotation]
		ifTrue: [
			(self isNewMethod: change)
				ifTrue: [
					String streamContents: [ :strm | | sel |
						sel := change methodSelector.
						strm
							nextPutAll: change changeClassName;
							nextPutAll: ' >> ';
							nextPutAll: sel;
							nextPutAll: ' is not present in the system. It has '.
							count := Smalltalk numberOfImplementorsOf: sel.
							count = 1
								ifTrue: [strm nextPutAll: '1 implementor']
								ifFalse: [count printOn: strm. strm nextPutAll: ' implementors' ].
							strm nextPutAll: ' and '.
							count := Smalltalk numberOfDirectSendersOf: sel.
							count = 1
								ifTrue: [strm nextPutAll: '1 direct sender.']
								ifFalse: [count printOn: strm. strm nextPutAll: ' direct senders.' ].
						]
					]
				ifFalse: [
					'current version: ', super annotation]]! !


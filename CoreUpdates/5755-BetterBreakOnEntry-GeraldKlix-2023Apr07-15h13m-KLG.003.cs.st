'From Cuis 6.0 [latest update: #5754] on 7 April 2023 at 3:26:38 pm'!
"Change Set:		5755-CuisCore-AuthorName-2023Apr07-15h13m
Date:			7 April 2023
Author:			Gerald Klix

Replace the BreakpointManager with a method wrapper."!

!classDefinition: #BreakingMethodWrapper category: #'Tools-Debugger'!
ProtoObject subclass: #BreakingMethodWrapper
	instanceVariableNames: 'method'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Debugger'!

!BreakingMethodWrapper methodsFor: 'system primitives' stamp: 'KLG 4/7/2023 13:01:44'!
doesNotUnderstand: aMessage
	"Let my method perform aMessage and answer the result."

	^ method
		perform: aMessage selector
		withArguments: aMessage arguments! !

!BreakingMethodWrapper methodsFor: 'system primitives' stamp: 'KLG 4/7/2023 12:59:56'!
run: aSelector with: anArray in: aReceiver
	"I am sent by the virtual machine, if an instance of a class other
	than CompiledMethod is bound in a method dictionary.
	
	See SystemDictionary>>#recreateSpecialObjectsArray at index 50."

	| process |
	process := Process 		
		forContext:
			(MethodContext
				sender: thisContext sender
				receiver: aReceiver
				method: method
				arguments: anArray)
			priority: Processor activeProcess 
		priority.	
	process	 debugWithTitle:
		'Breakpoint in ' , method methodClass name , '>>#' , method selector.	
	thisContext swapSender: nil.
	Processor activeProcess terminate! !

!BreakingMethodWrapper methodsFor: 'printing' stamp: 'KLG 4/7/2023 13:02:59'!
printAs: streamType limitedTo: limit
	"Answer an instance of streamType whose characters describe the receiver.

	If you want to print without a character limit, use fullPrintString."

	| limitedString |
	limitedString := streamType streamContents: [ :stream |
		self printOn: stream ] limitedTo: limit.
	limitedString size < limit ifTrue: [^ limitedString ].
	^ limitedString , '[..]'! !

!BreakingMethodWrapper methodsFor: 'printing' stamp: 'KLG 4/7/2023 13:03:10'!
printOn: aStream
	"Print a nice representation of me on aStream."

	aStream 
		nextPutAll: 'Break when entering ';
		print: method! !

!BreakingMethodWrapper methodsFor: 'printing' stamp: 'KLG 4/7/2023 13:03:16'!
printStringLimitedTo: limit
	"Answer a String whose characters are a description of the receiver.

	If you want to print without a character limit, use fullPrintString."

	^self printAs: String limitedTo: limit! !

!BreakingMethodWrapper methodsFor: 'printing' stamp: 'KLG 4/7/2023 13:03:25'!
printTextLimitedTo: limit
	"Answer a Text whose characters are a description of the receiver."

	^self printAs: Text limitedTo: limit! !

!BreakingMethodWrapper methodsFor: 'install/uninstall' stamp: 'KLG 4/7/2023 13:04:09'!
install
	"Install me as a replacement for my method."

	method methodClass methodDictionary
		at: method selector
		put: self! !

!BreakingMethodWrapper methodsFor: 'install/uninstall' stamp: 'KLG 4/7/2023 13:04:25'!
uninstall
	"Uninstall me."

	method methodClass methodDictionary
		at: method selector
		put: method! !

!BreakingMethodWrapper methodsFor: 'breakpoints' stamp: 'KLG 4/7/2023 13:04:50'!
hasBreakpoint
	"Answer true, because I am a breakpoint."

	^ true! !

!BreakingMethodWrapper methodsFor: 'initialization' stamp: 'KLG 4/7/2023 13:05:02'!
initializeOn: aCompiledMethod
	"Initialize me with aCompiledMethod."

	method := aCompiledMethod! !


!BreakingMethodWrapper class methodsFor: 'instance creation' stamp: 'KLG 4/7/2023 12:31:52'!
on: aCompiledMethod
	"Create an instance of mine on aCompiledMethod."

	^ self basicNew initializeOn: aCompiledMethod! !


!CodeProvider methodsFor: 'annotation' stamp: 'KLG 4/7/2023 15:26:24'!
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
					[#closuresInfo] -> [
						strm nextPutAll: (aClass closuresInfoAt: aSelector)].
					
				}]
			separatedBy: [ strm nextPutAll: self annotationSeparator ].
		(method notNil and: [method hasBreakpoint]) ifTrue: [
			strm nextPutAll: self annotationSeparator.
			strm nextPutAll: '[Breakpoint]' ]
	].! !


!Browser methodsFor: 'breakpoints' stamp: 'KLG 4/7/2023 13:20:41'!
toggleBreakOnEntry
	"Install or uninstall a halt-on-entry breakpoint"

	| selectedMethod |
	self selectedClassOrMetaClass ifNil: [ ^self ].
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	selectedMethod hasBreakpoint
		ifTrue:
			[ selectedMethod uninstall ]
		ifFalse:
			[ BreakingMethodWrapper on: selectedMethod :: install ].
	self
		changed: #messageList;
		changed: #annotation! !


!Debugger methodsFor: 'breakpoints' stamp: 'KLG 4/7/2023 13:50:31'!
toggleBreakOnEntry
	"Install or uninstall a halt-on-entry breakpoint"

	| selectedMethod |
	self selectedClassOrMetaClass ifNil: [ ^self ].
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	selectedMethod hasBreakpoint
		ifTrue:
			[ selectedMethod uninstall ]
		ifFalse:
			[ BreakingMethodWrapper on: selectedMethod :: install ].
	self changed: #annotation! !


!CompiledMethod methodsFor: 'breakpoints' stamp: 'KLG 4/7/2023 12:26:19'!
hasBreakpoint
	"In the standard image only BreakingMethodWrapper instances have breakpoints."
	
	^false! !


!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 4/5/2023 14:58:20'!
messageListMenu
	"Answer the message-list menu"

	^DynamicMenuBuilder buildTitled: 'Message List' targeting: self collectingMenuOptionsWith: #messageListMenuOptions.
	! !

!methodRemoval: Object #break stamp: 'KLG 4/7/2023 15:16:05'!
Object removeSelector: #break!

!BreakingMethodWrapper reorganize!
('system primitives' doesNotUnderstand: run:with:in:)
('printing' printAs:limitedTo: printOn: printStringLimitedTo: printTextLimitedTo:)
('install/uninstall' install uninstall)
('breakpoints' hasBreakpoint)
('initialization' initializeOn:)
!

!classRemoval: #BreakPoint stamp: 'KLG 4/7/2023 15:16:58'!
Smalltalk removeClassNamed: #BreakPoint!

!classRemoval: #BreakpointManager stamp: 'KLG 4/7/2023 15:16:42'!
Smalltalk removeClassNamed: #BreakpointManager!


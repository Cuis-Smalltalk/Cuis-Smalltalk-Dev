'From Cuis 5.0 [latest update: #4879] on 23 September 2021 at 10:21:47 am'!

!CodeProvider methodsFor: 'annotation' stamp: 'jmv 9/22/2021 18:29:08'!
annotationForSystemCategory: aCategory
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class."

	| separator |
	separator _ self annotationSeparator.
	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'System Category';
			nextPutAll: aCategory;
			nextPutAll: separator;
			print: (SystemOrganization listAtCategoryNamed: aCategory) size;
			nextPutAll: ' classes';
			nextPutAll: separator;
			print: (SystemOrganization instanceMethodCountOf: aCategory);
			nextPutAll: ' instance methods';
			nextPutAll: separator;
			print: (SystemOrganization classMethodCountOf: aCategory);
			nextPutAll: ' class methods';
			nextPutAll: separator;
			print: (SystemOrganization linesOfCodeOf: aCategory);
			nextPutAll: ' total lines of code' ]! !


!SystemOrganizer methodsFor: 'source code management' stamp: 'jmv 9/23/2021 10:18:03'!
classMethodCountOf: category

	^ (self superclassOrderIn: category) sum: [ :cl | cl class selectors size ] ifEmpty: 0.! !

!SystemOrganizer methodsFor: 'source code management' stamp: 'jmv 9/23/2021 10:18:08'!
instanceMethodCountOf: category

	^ (self superclassOrderIn: category) sum: [ :cl | cl selectors size ] ifEmpty: 0.! !

!SystemOrganizer methodsFor: 'source code management' stamp: 'jmv 9/23/2021 10:16:49'!
linesOfCodeOf: category
"
SystemOrganization linesOfCodeOf: #'System-Files'
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."

	^ (self superclassOrderIn: category) sum: [ :cl | cl linesOfCode ] ifEmpty: 0.! !


!CodeProvider methodsFor: 'annotation' stamp: 'jmv 9/22/2021 18:45:23'!
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
	separator _ self annotationSeparator.
	requestList
		do: [:aRequest | 
			aRequest == #firstComment
				ifTrue: [
					aComment _ aClass firstCommentAt: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream position = 0 ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aComment]].
			aRequest == #masterComment
				ifTrue: [
					aComment _ aClass supermostPrecodeCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream position = 0 ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aComment]].
			aRequest == #documentation
				ifTrue: [
					aComment _ aClass precodeCommentOrInheritedCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream isEmpty ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aComment]].
			aRequest == #timeStamp
				ifTrue: [
					stamp _ self timeStamp.
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					stream nextPutAll: (stamp size > 0 ifTrue: [stamp] ifFalse: ['no timestamp'])].
			aRequest == #linesOfCode
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					stream nextPutAll: ((aClass compiledMethodAt: aSelector ifAbsent: nil)
						ifNotNil: [ :cm | cm linesOfCode]) printString, ' lines of code'].
			aRequest == #messageCategory
				ifTrue: [
					aCategory _ aClass organization categoryOfElement: aSelector.
					aCategory
						ifNotNil: ["woud be nil for a method no longer present,  
							e.g. in a recent-submissions browser"
							stream isEmpty ifFalse: [stream nextPutAll: separator].
							stream nextPutAll: aCategory]].
			aRequest == #sendersCount
				ifTrue: [
					sendersCount _ Smalltalk numberOfSendersOf: aSelector.
					sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					stream nextPutAll: sendersCount].
			aRequest == #implementorsCount
				ifTrue: [
					implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
					implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					stream nextPutAll: implementorsCount].
			aRequest == #priorVersionsCount
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
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
						ifNotNil: [stream isEmpty ifFalse: [stream nextPutAll: separator].
							stream nextPutAll: 'prior timestamp: ' , stamp]].
			aRequest == #packages
				ifTrue: [
					(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
						stream isEmpty ifFalse: [stream nextPutAll: separator].
						(CodePackage packageOfMethod: cm methodReference ifNone: nil)
							ifNil: [ stream nextPutAll: 'in no package' ]
							ifNotNil: [ :codePackage |
								stream nextPutAll: 'in package '; nextPutAll: codePackage packageName ]]].
			aRequest == #changeSets
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set']
								ifFalse: [stream nextPutAll: 'in change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no change set']].
			aRequest == #allChangeSets
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set']
								ifFalse: [stream nextPutAll: 'in change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no change set']].
			aRequest == #allBaseSystemChangeSets
				ifTrue: [
					stream isEmpty ifFalse: [stream nextPutAll: separator].
					aList _ (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
					aList size > 0
						ifTrue: [
							aList size = 1
								ifTrue: [stream nextPutAll: 'only in base system change set']
								ifFalse: [stream nextPutAll: 'in base system change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no base system change set']].
			aRequest == #closuresInfo
				ifTrue: [
					aString _ aClass closuresInfoAt: aSelector.
					aString size > 0
						ifTrue: [stream isEmpty ifFalse: [stream nextPutAll: separator].
								stream nextPutAll: aString]].
		].
	^ stream contents! !


!Browser methodsFor: 'annotation' stamp: 'jmv 9/23/2021 10:08:33'!
annotation
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |
	(aClass _ self selectedClassOrMetaClass)
		ifNil: [
			self selectedSystemCategoryName ifNotNil: [ :sysCat |
				^self annotationForSystemCategory: sysCat ].
			^ ''].
	self editSelection == #editComment
		ifTrue: [^ self annotationForSelector: #Comment ofClass: aClass].
	self isEditingExistingClass 
		ifTrue: [^ self annotationForSelector: #Definition ofClass: aClass].
	(aSelector _ self selectedMessageName)
		ifNil: [^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass! !


!ClassDescription methodsFor: 'source code management' stamp: 'jmv 9/23/2021 10:05:44'!
linesOfCode
"
Object linesOfCode
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines.
	If asked to a class, also include its metaclass (i.e. the class side).
	If asked to a metaclass (the class side), don't include the class (the instance side)."

	| lines |
	lines _ 0.
	self selectorsDo: [ :sel |
		lines _ lines + (self compiledMethodAt: sel) linesOfCode ].
	^self isMeta
		ifTrue: [ lines]
		ifFalse: [ lines + self class linesOfCode].
"
(SystemOrganization categories select: [:c | 'Kernel*' match: c]) sum: [:c |
	(SystemOrganization superclassOrderIn: c) sum: [:cl | cl linesOfCode]]
"
"
Smalltalk allClasses sum: [:cl | cl linesOfCode]
"! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 9/23/2021 10:03:40'!
linesOfCode
	"An approximate measure of lines of code.
	Use decompiled source code. In this way, the measure:
	- Doesn't include comments
	- Doesn't include blank lines
	- Is not sensitive to code formatting
	The motivation is to consider LOC as an expense, not an asset. Minimizing LOC is good.
	But it is not like that for comments!!"

	| lines |
	lines _ 0.
	self decompileString lineIndicesDo: [ :start :endWithoutDelimiters :end |
		endWithoutDelimiters - start > 0 ifTrue: [
			lines _ lines+1 ]].
	^lines! !


!Preferences class methodsFor: 'parameters' stamp: 'jmv 9/22/2021 18:41:13'!
annotationInfo 
	"Answer a list of pairs characterizing all the available kinds of annotations; in each pair, the first element is a symbol representing the info type, and the second element is a string providing the corresponding balloon help"

	^ #(
		(timeStamp					'The time stamp of the last submission of the method.')
		(firstComment					'The first comment in the method, if any.')
		(masterComment					'The comment at the beginning of the supermost implementor of the method if any.')
		(documentation					'Comment at beginning of the method or, if it has none, comment at the beginning of a superclass''s implementation of the method.')
		(messageCategory					'Which method category the method lies in.')
		(sendersCount					'A report of how many senders there of the message.')
		(implementorsCount					'A report of how many implementors there are of the message.')
		(allChangeSets					'A list of all change sets bearing the method.')
		(priorVersionsCount					'A report of how many previous versions there are of the method.')
		(priorTimeStamp					'The time stamp of the penultimate submission of the method, if any.')
		(closuresInfo					'Details about BlockClosures in the method.')
		(packages					'Details about CodePackages including the method.')
		(linesOfCode					'Number of lines of code, including comments but not blank lines.')
	)! !

!CodePackage methodsFor: 'source code management' stamp: 'jmv 9/23/2021 10:27:42'!
linesOfCode
	"An approximate measure of lines of code.
	Does not includes comments, or excludes blank lines.
	See comment at CompiledMethod >> #linesOfCode"

	^self methods inject: 0 into: [ :sum :each |
		sum + each compiledMethod linesOfCode ].! !

!Preferences class methodsFor: 'parameters' stamp: 'jmv 9/22/2021 18:42:21'!
setDefaultAnnotationInfo
	"
	Preferences setDefaultAnnotationInfo
	"
	^ self parameters at: #MethodAnnotations put: #(timeStamp linesOfCode messageCategory implementorsCount sendersCount  packages changeSets)! !


!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment theMetaClass theNonMetaClass)
('accessing class hierarchy' classesThatImplementAllOf: removeUninstantiatedSubclassesSilently subclasses)
('accessing method dictionary' addAndClassifySelector:withMethod:inProtocol:notifying: allMethodCategoriesIntegratedThrough: changeRecordsAt: induceMDFault methodCategories methodCategoriesAsSortedCollection methodsInCategory: recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem:)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:notifying: compile:classified:withStamp:notifying:logSource: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: localBindingOf: moveInstVarNamed:to:after: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentFollows commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:priorSource:inFile: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on: printCategoryChunk:on:priorMethod: printCategoryChunk:on:withStamp:priorMethod: printCategoryChunk:on:withStamp:priorMethod:overridesMethod: printCategoryChunk:withStamp:on: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' doneCompiling superclass:methodDictionary:format: updateInstancesFrom:)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: instVarNames removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization organization: whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:notifying: newInstanceFrom:variable:size:map: setInstVarNames:)
('accessing instances and variables' allUnreferencedInstanceVariables)
('pool variables' classPool)
('source code management' linesOfCode)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Preferences setDefaultAnnotationInfo!


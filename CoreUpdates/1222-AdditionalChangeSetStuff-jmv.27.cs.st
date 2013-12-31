'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 25 March 2012 at 10:23:54 pm'!

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 3/22/2012 16:51'!
                             noteClassMoveToOtherPackage: class
	"The class is about to be moved to some other package, who will hold it.
	Adjust the receiver to reflect that fact."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #movedToOtherPackage.
	changeRecords removeKey: class class name ifAbsent: nil! !

!ChangeSet methodsFor: 'change logging' stamp: 'jmv 3/22/2012 17:11'!
   noteMethodMoveToOtherPackage: selector forClass: class

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteMethodMoveToOtherPackage: selector! !


!ClassChangeRecord methodsFor: 'method changes' stamp: 'jmv 3/22/2012 17:10'!
                           noteMethodMoveToOtherPackage: selector

	| methodChange |
	methodChange _ self findOrMakeMethodChangeAt: selector priorMethod: nil.
	methodChange noteChangeType: #movedToOtherPackage! !


!ChangeSet methodsFor: 'accessing' stamp: 'jmv 3/23/2012 14:23'!
   classMovesToOtherPackage
	"Unlike some related methods, answer an Array (not a Set)"
	^ changeRecords keys select: [ :className |
		(changeRecords at: className) isClassMoveToOtherPackage]! !

!ChangeSet methodsFor: 'method changes' stamp: 'jmv 3/23/2012 20:08'!
                         changedMessageList
	"Used by a message set browser to access the list view information."

	| messageList |
	messageList _ OrderedCollection new.
	changeRecords associationsDo: [ :clAssoc | | classNameInFull classNameInParts |
		classNameInFull _ clAssoc key asString.
		classNameInParts _ classNameInFull findTokens: ' '.

		(clAssoc value allChangeTypes includes: #comment) ifTrue: [
			messageList add:
				(MethodReference new
					setClassSymbol: classNameInParts first asSymbol
					classIsMeta: false 
					methodSymbol: #Comment 
					stringVersion: classNameInFull, ' Comment')].

		clAssoc value methodChangeTypes associationsDo: [ :mAssoc |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				messageList add:
					(MethodReference new
						setClassSymbol: classNameInParts first asSymbol
						classIsMeta: classNameInParts size > 1 
						methodSymbol: mAssoc key 
						stringVersion: classNameInFull, ' ' , mAssoc key)]]].
	^ messageList asSortedArray! !

!ChangeSet methodsFor: 'moving changes' stamp: 'jmv 3/23/2012 20:10'!
         methodsWithAnyInitialsOtherThan: myInits
	"Return a collection of method refs whose author appears to be different from the given one, even historically"
	| slips |
	slips _ Set new.
	self changedClasses do: [:aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | method |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				method _ aClass compiledMethodAt: mAssoc key ifAbsent: nil.
				method ifNotNil: [
					(aClass changeRecordsAt: mAssoc key) do: [ :chg | | aTimeStamp |
						aTimeStamp _ chg stamp.
						(aTimeStamp notNil and: [(aTimeStamp beginsWith: myInits) not])
							ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]]].
	^ slips! !

!ChangeSet methodsFor: 'moving changes' stamp: 'jmv 3/23/2012 20:10'!
        methodsWithInitialsOtherThan: myInits
	"Return a collection of method refs whose author appears to be different from the given one"
	| slips |
	slips _ OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | method |
				(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
					method _ aClass compiledMethodAt: mAssoc key ifAbsent: nil.
					method ifNotNil: [ | aTimeStamp |
						((aTimeStamp _ Utilities timeStampForMethod: method) notNil and: [
							(aTimeStamp beginsWith: myInits) not])
								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithInitialsOtherThan: 'sw') name: 'authoring problems'
	"! !

!ChangeSet methodsFor: 'moving changes' stamp: 'jmv 3/23/2012 20:10'!
          methodsWithoutComments
	"Return a collection representing methods in the receiver which have no precode comments"

	| slips |
	slips _ OrderedCollection new.
	self changedClasses do:
		[:aClass |
		(self methodChangesAtClass: aClass name) associationsDo: 
				[:mAssoc | (#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse:
					[(aClass selectors includes:  mAssoc key) ifTrue:
						[(aClass firstPrecodeCommentFor: mAssoc key) isEmptyOrNil
								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithoutComments) name: 'methods lacking comments'
	"! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/23/2012 20:08'!
                         checkForSlips
	"Return a collection of method refs with possible debugging code in them."
	| slips |
	slips _ OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc |  | method |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				method _ aClass compiledMethodAt: mAssoc key ifAbsent: nil.
				method ifNotNil: [
					method hasReportableSlip
						ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/23/2012 14:17'!
          fileOutMethodChangesFor: class on: stream
	"Write out all the method changes for this class."

	| changes |
	changes _ Set new.
	(self methodChangesAtClass: class name) associationsDo: [ :mAssoc |
		(mAssoc value == #remove
			or: [ mAssoc value == #addedThenRemoved
				or: [ mAssoc value == #add
					or: [ mAssoc value == #movedToOtherPackage ]]])
			ifFalse: [ changes add: mAssoc key ]].
	changes isEmpty ifFalse: [
		class fileOutChangedMessages: changes on: stream.
		stream newLine ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/23/2012 19:49'!
              fileOutMethodMovedToOtherPackagesFor: class on: stream 
	"Write out removals and initialization for this class."

	| dict classRecord |
	classRecord _ changeRecords at: class name ifAbsent: [^ self].
	dict _ classRecord methodChangeTypes.
	dict keysSortedSafely do: [ :key | | changeType |
		changeType _ dict at: key.
		(#(movedToOtherPackage) includes: changeType)
			ifTrue: [
				stream nextPut: $!!; nextChunkPut: 'methodMoveToSomePackage: ', class name, ' ', key storeString; newLine.
				stream nextChunkPut: class name, ' removeSelectorIfInBaseSystem: ', key storeString; newLine ].
		].! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/23/2012 19:49'!
      fileOutOn: stream 
	"Write out all the changes the receiver knows about"

	| classList |
	(self isEmpty and: [ stream isKindOf: FileStream ])
		ifTrue: [ self inform: 'Warning: no changes to file out' ].
	classList _ ChangeSet superclassOrder: self changedClasses asOrderedCollection.

	"First put out rename, max classDef and comment changes."
	classList do: [ :aClass | self fileOutClassDefinition: aClass on: stream ].

	"Then put out all the method additions"
	classList do: [ :aClass | self fileOutMethodAdditionsFor: aClass on: stream ].

	"Then put out all the method changes"
	classList do: [ :aClass | self fileOutMethodChangesFor: aClass on: stream ].

	"Finally put out removals, final class defs and reorganization if any"
	classList reverseDo: [ :aClass |
		self fileOutMethodRemovalsFor: aClass on: stream.
		self fileOutMethodMovedToOtherPackagesFor: aClass on: stream.
		self fileOutPSFor: aClass on: stream ].

	self classRemoves sort do: [ :aClassName |
		stream nextPut: $!!; nextChunkPut: ('classRemoval: #', aClassName); newLine.
		stream nextChunkPut: 'Smalltalk removeClassNamed: #', aClassName; newLine ].

	self classMovesToOtherPackage sort do: [ :aClassName |
		stream nextPut: $!!; nextChunkPut: ('classMoveToSomePackage: #', aClassName); newLine.
		stream nextChunkPut: 'Smalltalk removeClassNamedIfInBaseSystem: #', aClassName; newLine ]! !


!ClassChangeRecord methodsFor: 'all changes' stamp: 'jmv 3/22/2012 16:40'!
                        noteChangeType: changeSymbol fromClass: class

	changeSymbol == #movedToOtherPackage ifTrue: [
		^ changeTypes add: changeSymbol].
	"Any other change type meanse we're still here!!"
	changeTypes remove: #movedToOtherPackage ifAbsent: nil.

	(changeSymbol == #new or: [changeSymbol == #add]) ifTrue: [
		changeTypes add: #add.
		changeTypes remove: #change ifAbsent: nil.
		^ self].
	changeSymbol == #change ifTrue: [
		(changeTypes includes: #add) ifTrue: [^ self].
		^ changeTypes add: changeSymbol].
	changeSymbol == #addedThenRemoved ifTrue: [
		^ self].  "An entire class was added but then removed"
	changeSymbol == #comment ifTrue: [
		^ changeTypes add: changeSymbol].
	changeSymbol == #reorganize ifTrue: [
		^ changeTypes add: changeSymbol].
	changeSymbol == #rename ifTrue: [
		^ changeTypes add: changeSymbol].
	(changeSymbol beginsWith: 'oldName: ') ifTrue: [
		"Must only be used when assimilating other changeSets"
		(changeTypes includes: #add) ifTrue: [^ self].
		priorName _ changeSymbol copyFrom: 'oldName: ' size + 1 to: changeSymbol size.
		^ changeTypes add: #rename].
	changeSymbol == #remove ifTrue: [
		(changeTypes includes: #add)
			ifTrue: [changeTypes add: #addedThenRemoved]
			ifFalse: [changeTypes add: #remove].
		^ changeTypes removeAllFoundIn: #(add change comment reorganize)].

	self error: 'Unrecognized changeType'! !

!ClassChangeRecord methodsFor: 'definition' stamp: 'jmv 3/22/2012 16:35'!
         checkCoherence
	"If I recreate the class then don't remove it"

	(changeTypes includes: #remove) ifTrue: [
		changeTypes remove: #remove.
		changeTypes add: #change ].
	(changeTypes includes: #addedThenRemoved) ifTrue: [
		changeTypes remove: #addedThenRemoved.
		changeTypes add: #add ].
	(changeTypes includes: #movedToOtherPackage) ifTrue: [
		changeTypes remove: #movedToOtherPackage.
		changeTypes add: #add ].! !

!ClassChangeRecord methodsFor: 'removal' stamp: 'jmv 3/23/2012 14:23'!
                     isClassMoveToOtherPackage
	"NOTE: there are other removals with changeType #addedThenRemoved,
	but this message is used to write out removals in fileOut, and those
	cases should not be written out."

	^ changeTypes includes: #movedToOtherPackage! !


!ClassDeletionChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/23/2012 19:48'!
               fileIn

	doItOnlyIfInBaseSystem
		ifTrue: [
			Smalltalk removeClassNamedIfInBaseSystem: self methodClass name]
		ifFalse: [
			self methodClass removeFromSystem ]! !

!ClassDeletionChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/23/2012 19:46'!
  fileOutOn: stream
	"File the receiver out on the given file stream"

	doItOnlyIfInBaseSystem
		ifTrue: [
			stream nextPut: $!!; nextChunkPut: ('classMoveToSomePackage: #', clsName); newLine.
			stream nextChunkPut: 'Smalltalk removeClassNamedIfInBaseSystem: #', clsName; newLine ]
		ifFalse: [
			stream nextPut: $!!; nextChunkPut: ('classRemoval: #', clsName); newLine.
			stream nextChunkPut: 'Smalltalk removeClassNamed: #', clsName; newLine ]! !

!ClassDeletionChangeRecord methodsFor: 'accessing' stamp: 'jmv 3/23/2012 19:37'!
           doItOnlyIfInBaseSystem: aBoolean
	doItOnlyIfInBaseSystem _ aBoolean! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 3/23/2012 19:50'!
                             removeSelectorIfInBaseSystem: selector
	"Remove the message whose selector is given from the method 
	dictionary of the receiver, if it is there and not part of a package. Answer nil otherwise."
	| priorProtocol | 

	self compiledMethodAt: selector ifAbsent: [^ nil].
	priorProtocol _ self whichCategoryIncludesSelector: selector.
	CodePackage
		packageOfMethodCategory: priorProtocol
		ofClass: self
		ifNone: [
			"If remove is actually done, then include it in the current change set for the base system,
			as a regular remove."
			self removeSelector: selector ]! !


!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/23/2012 18:37'!
          classRecategorized: aClass from: oldCategory to: newCategory
	"If the class was moved to a dfferent package, out of the base system, record the fact in the change set.
	The actual class redefinition is done at #classDefinitionChangedFrom:to: that is also called."

	"If destination is not a package, just exit."
	CodePackage
		packageOfSystemCategory: newCategory
		ifNone:	[ ^self ].

	"If source is not a package (i.e. it is the base system) then record the change."
	CodePackage
		packageOfSystemCategory: oldCategory
		ifNone: [
			self changeSetForBaseSystem noteClassMoveToOtherPackage: aClass ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/23/2012 18:39'!
         selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass
	"If the mathod was moved to a dfferent package, affect the package that lost the it. Tell it that it lost the method.
	The actual method redefinition is done at one of the method definition methods, that is also called."

	"If destination is not a package, just exit."
	CodePackage
		packageOfMethodCategory: newCategory
		ofClass: aClass
		ifNone: [ ^self ].

	"If source is not a package (i.e. it is the base system) then record the change."
	CodePackage
		packageOfMethodCategory: oldCategory
		ofClass: aClass
		ifNone: [
			self changeSetForBaseSystem noteMethodMoveToOtherPackage: selector forClass: aClass]! !


!CodeFile methodsFor: 'initialize' stamp: 'jmv 3/23/2012 14:03'!
 fromFileNamed: aName
	| stream |
	fullName_ aName.
	stream _ FileStream readOnlyFileNamed: aName.
	[ self buildFrom: stream ] ensure: [ stream close ]! !

!CodeFile methodsFor: 'private' stamp: 'jmv 3/23/2012 20:03'!
       removedMethod: string with: chgRec
	| class tokens firstToken secondToken thirdToken |
	tokens _ Smalltalk actualScannerClass new scanTokens: string.
	tokens size >= 3 ifTrue: [
		firstToken _ tokens at: 1.
		secondToken _ tokens at: 2.
		thirdToken _tokens at: 3.
		(tokens size = 3 and: [ secondToken == #removeSelector: or: [ secondToken == #removeSelectorIfInBaseSystem: ]]) ifTrue:[
			class _ self getClass: firstToken.
			^class perform: secondToken with: thirdToken.
		].
		(tokens size = 4 and: [ secondToken == #class and: [ thirdToken == #removeSelector: or: [ thirdToken == #removeSelectorIfInBaseSystem: ]]]) ifTrue:[
			class _ self getClass: firstToken.
			^class metaClass perform: thirdToken with: (tokens at: 4).
		].
	].
	doIts add: chgRec! !

!CodeFile methodsFor: 'reading' stamp: 'jmv 3/23/2012 20:03'!
       buildFrom: aStream
	| chgRec changes |
	changes _ (ChangeList new scanFile: aStream from: 0 to: aStream size) changeList.
	('Processing ', self name) 
		displayProgressAt: Sensor mousePoint
		from: 1
		to: changes size
		during: [ :bar |
			1 to: changes size do:[:i|
				bar value: i.
				chgRec := changes at: i.
				chgRec class == MethodDeletionChangeRecord
					ifTrue: [ self removedMethod: chgRec command with: chgRec ]
					ifFalse: [ self perform: (chgRec type copyWith: $:) asSymbol with: chgRec ].
			].
		]! !


!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 3/23/2012 14:03'!
  browseStream: aStream named: aString

	| codeFile organizer browser |
	Cursor wait showWhile: [
		organizer _ SystemOrganizer defaultList: Array new.
		codeFile _ (CodeFile new fullName: aString; buildFrom: aStream).
		aStream close.
		organizer 
			classifyAll: codeFile classes keys 
			under: codeFile name.
		(browser _ CodeFileBrowser new)
			systemOrganizer: organizer;
			codeFile: codeFile].
	CodeFileBrowserWindow open: browser label: 'Code File Browser'! !


!CodePackageFile methodsFor: 'initialize' stamp: 'jmv 3/23/2012 14:03'!
   buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	"Just build the PackageFile object. Don't install the code."

	| classesDefined classesExtended classesToDeleteButCant classesToReallyDelete packageInMemory |
	packageName _pkName.
	fullName _fullFileName.
	"Don't register a package!!"
	packageInMemory _ (CodePackage named: packageName)
		ifNil: [CodePackage newNamed: packageName].
	self buildFrom: aFileStream.
	"Compute stuff no longer in package: Should be removed from system."
	classesDefined _ Set new.
	classesExtended _ Set new.
	self classes do: [ :pseudoClass |
		pseudoClass hasDefinition
			ifTrue: [ classesDefined add: pseudoClass name ]
			ifFalse: [ classesExtended add: pseudoClass name ]].
	classesToRemove _ (packageInMemory classes asSet collect: [ :each | each name ]) difference: classesDefined.
	"Add here:
		- classes in classesToDelete, that #allCallsOn answers selectors that aren't in classesToDelete or methodsToRemove
		- classes with #subclasses that aren't in classesToDelete.
		- classes with existing instances (#instanceCount)? Not really sure... Maybe sole instance referenced from classVar or such...
		- something else I forgot?
	Warning: This search for stuff that can't be removed must be iterated again until it doesn't find any more."
	classesToDeleteButCant _ classesToRemove intersection: classesExtended.
	classesToReallyDelete _ classesToRemove difference: classesToDeleteButCant.
	"Methods. Could also mean classes that can't be deleted!! (include in the iteration)
	Warn if deleting last implementor of sent messages?"
	methodsToRemove _ packageInMemory methods asSet difference: self allMethodReferences.
	methodsToRemove _ methodsToRemove reject: [ :methodReference | classesToReallyDelete includes: methodReference classSymbol ].

"
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	('classesToDeleteButCant: ', classesToDeleteButCant printString) print.
	('classesToReallyDelete: ', classesToReallyDelete printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
"! !


!CodeProvider methodsFor: 'categories' stamp: 'jmv 3/25/2012 22:21'!
                  categoryFromUserWithPrompt: aPrompt for: aClass
	"self new categoryFromUserWithPrompt: 'testing' for: SystemDictionary"

	|  labels myCategories reject lines newName menuIndex |
	labels _ OrderedCollection with: 'new...'.
	labels addAll: (myCategories _ aClass organization categories asArray copy sort:
		[ :a :b | a asLowercase < b asLowercase ]).
	reject _ myCategories asSet.
	reject
		add: ClassOrganizer nullCategory;
		add: ClassOrganizer default.
	lines _ OrderedCollection with: 1 with: (myCategories size + 1).

	aClass allSuperclasses do: [ :cls | | cats |
			cats _ cls organization categories reject: [ :cat | reject includes: cat].
			cats isEmpty ifFalse: [
				lines add: labels size.
				labels addAll: (cats asArray sort: [ :a :b | a asLowercase < b asLowercase]).
				reject addAll: cats]].

	(labels size = 1 or: [
		menuIndex _ (PopUpMenu labelArray: labels lines: lines)
		startUpWithCaption: aPrompt.
		menuIndex = 0 ifTrue: [^ nil].
		menuIndex = 1])
			ifTrue:[
				newName _ FillInTheBlank request: 'Please type new category name' initialAnswer: 'category name'.
				newName isEmpty ifTrue: [ ^nil ]]
			ifFalse: [ newName _ labels at: menuIndex ].
	^ newName ifNotNil: [ newName asSymbol ]! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 3/23/2012 20:05'!
                         scanCategory
	"Scan anything that involves more than one chunk; method name is historical only"

	| itemPosition item item2 tokens firstToken secondToken stamp isComment anIndex def isMeta name record methodReference doItOnlyIfInBaseSystem |
	itemPosition _ file position.
	item _ file nextChunk.
	isComment _ (item includesSubString: 'commentStamp:').
	((isComment
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
			^file skipStyleChunk ].

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
			^file skipStyleChunk ].

		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem_ firstToken == #methodMoveToSomePackage:.
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
			^file skipStyleChunk ].
		
		secondToken == #methodsFor: ifTrue: [
			^ self scanCategory: tokens third class: firstToken meta: false stamp: stamp].

		tokens third == #methodsFor: ifTrue: [
			^ self scanCategory: tokens fourth class: firstToken meta: true stamp: stamp]].

	secondToken == #commentStamp:
		ifTrue: [
			stamp _ tokens third.
			self addItem:
					(ChangeRecord new file: file position: file position type: #classComment
									class: firstToken category: nil meta: false stamp: stamp)
					text: 'class comment for ' , firstToken, 
						  (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).
			file nextChunk.
			^ file skipStyleChunk]! !


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 3/22/2012 17:45'!
                acceptedStringOrText
	"return the source code that shows in the bottom pane"

	| sel class strm changeType answer |
	self changed: #clearUserEdits.
	currentClassName ifNil: [^ myChangeSet preambleString ifNil: ['']].
	class _ self selectedClassOrMetaClass.
	(sel _ currentSelector)
		ifNotNil: [
			changeType _ (myChangeSet atSelector: (sel _ sel asSymbol) class: class).
			changeType == #remove
				ifTrue: [^'Method has been removed (see versions)'].
			changeType == #addedThenRemoved
				ifTrue: [^'Added then removed (see versions)'].
			changeType == #movedToOtherPackage
				ifTrue: [^'Method was moved to some other package'].
			class ifNil: [^'Method was added, but cannot be found!!'].
			(class includesSelector: sel)
				ifFalse: [^'Method was added, but cannot be found!!'].
			answer _  (class sourceCodeAt: sel).
			(#(prettyPrint prettyLineDiffs prettyWordDiffs) includes: contentsSymbol) ifTrue: [
				answer _ (class compilerClass new
						format: answer
						in: class 
						notifying: nil)].
			self showingAnyKindOfDiffs
				ifTrue: [ answer _ (self diffFromPriorSourceFor: answer) ].
			^ answer asText makeSelectorBoldIn: class ]
		ifNil: [
			strm _ WriteStream on: (String new: 100).
			(myChangeSet classChangeAt: currentClassName) do: [ :each |
				each == #remove ifTrue: [strm nextPutAll: 'Entire class was removed.'; newLine].
				each == #addedThenRemoved ifTrue: [strm nextPutAll: 'Class was added then removed.'; newLine].
				each == #rename ifTrue: [strm nextPutAll: 'Class name was changed.'; newLine].
				each == #add ifTrue: [strm nextPutAll: 'Class definition was added.'; newLine].
				each == #change ifTrue: [strm nextPutAll: 'Class definition was changed.'; newLine].
				each == #reorganize ifTrue: [strm nextPutAll: 'Class organization was changed.'; newLine].
				each == #comment ifTrue: [strm nextPutAll: 'New class comment.'; newLine].
				each == #movedToOtherPackage ifTrue: [strm nextPutAll: 'Class was moved to some other package.'; newLine].
			].
			^ strm contents].! !


!MethodDeletionChangeRecord methodsFor: 'accessing' stamp: 'jmv 3/23/2012 19:37'!
                           doItOnlyIfInBaseSystem: aBoolean
	doItOnlyIfInBaseSystem _ aBoolean! !

!MethodDeletionChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/23/2012 19:55'!
 fileIn

	doItOnlyIfInBaseSystem
		ifTrue: [ methodReference actualClass removeSelectorIfInBaseSystem: self methodSelector ]
		ifFalse: [ methodReference actualClass removeSelector: self methodSelector ]! !

!MethodDeletionChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/23/2012 19:56'!
                          fileOutOn: stream
	"File the receiver out on the given file stream"
	
	doItOnlyIfInBaseSystem
		ifTrue: [
			stream nextPut: $!!; nextChunkPut: 'methodMoveToSomePackage: ', self methodClassName, ' ', self methodSelector; newLine.
			stream nextChunkPut: self command; newLine ]
		ifFalse: [
			stream nextPut: $!!; nextChunkPut: 'methodRemoval: ', self methodClassName, ' ', self methodSelector; newLine.
			stream nextChunkPut: self command; newLine ]! !

!MethodDeletionChangeRecord methodsFor: 'services' stamp: 'jmv 3/23/2012 19:56'!
      command

	^doItOnlyIfInBaseSystem
		ifTrue: [ self methodClassName, ' removeSelectorIfInBaseSystem: ', self methodSelector ]
		ifFalse: [ self methodClassName, ' removeSelector: ', self methodSelector ]! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 3/23/2012 20:03'!
     fileInAnnouncing: announcement 
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: 
			[:bar | 
			[self atEnd] whileFalse: 
					[bar value: self position.
					self skipSeparators.
					
					[val := (self peekFor: $!!) 
								ifTrue: [
									chunk := self nextChunk.
									"These are the ones that should do nothing, because next line is a doit that does the stuff"
									(chunk beginsWith: 'classDefinition: ')
									| (chunk beginsWith: 'classRemoval: ')
									| (chunk beginsWith: 'methodRemoval: ')
									| (chunk beginsWith: 'classMoveToSomePackage: ')
									| (chunk beginsWith: 'methodMoveToSomePackage: ')
										ifFalse: [(Smalltalk actualCompilerClass evaluate: chunk logged: false) scanFrom: self]]
								ifFalse: [
									chunk := self nextChunk.
									self checkForPreamble: chunk.
									Smalltalk actualCompilerClass evaluate: chunk logged: true]] 
							on: InMidstOfFileinNotification
							do: [:ex | ex resume: true].
					self skipStyleChunk].
			self close].
	"Note:  The main purpose of this banner is to flush the changes file."
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 3/22/2012 22:32'!
 removeClassNamed: aName
	"Invoked from fileouts:  if there is currently a class in the system named aName, then remove it.  If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	oldClass _ self at: aName asSymbol ifAbsent: [
		Transcript newLine; show: 'Removal of class named ', aName, ' ignored because ', aName, ' does not exist.'.
		^ self].

	oldClass removeFromSystem! !

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 3/23/2012 18:45'!
    removeClassNamedIfInBaseSystem: aName
	"Invoked from fileouts:  if there is currently a class in the system named aName, and it is not part of a package, then remove it.
	If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	oldClass _ self at: aName asSymbol ifAbsent: [
"		Transcript newLine; show: 'Removal of class named ', aName, ' ignored because ', aName, ' does not exist.'."
		^ self].

	CodePackage
		packageOfClass: oldClass
		ifNone: [
			"If remove is actually done, then include it in the current change set for the base system,
			as a regular remove."
			oldClass removeFromSystem ]! !


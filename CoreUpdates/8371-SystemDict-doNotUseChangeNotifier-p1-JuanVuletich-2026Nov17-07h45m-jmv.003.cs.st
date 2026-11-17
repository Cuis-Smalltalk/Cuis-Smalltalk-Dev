'From Cuis7.9 [latest update: #8370] on 17 November 2026 at 8:08:15 am'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 17/Nov/2026, 10:58:16 am (UTC)'!
logClassRemoval: aClass
	
	aClass acceptsLoggingOfCompilation
		ifTrue: [
			self
				logPreamble: 'classRemoval: ', aClass name printString, Utilities changeStampField
				change: aClass definition ].! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 17/Nov/2026, 10:52:48 am (UTC)'!
logClassRemoved: aClass
	
	aClass acceptsLoggingOfCompilation
		ifTrue: [
			self
				logPreamble: 'classRemoval: ', aClass name printString, Utilities changeStampField
				change: aClass definition ].! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 17/Nov/2026, 10:57:33 am (UTC)'!
logMethodRemoval: aMethod selector: aSymbol class: aClass

	aClass acceptsLoggingOfCompilation
		ifTrue: [
			self
				logPreamble: 'methodRemoval: ', aClass name, ' ', aSymbol storeString, Utilities changeStampField
				change: aMethod sourceCode ].! !


!Compiler methodsFor: 'public access' stamp: 'jmv 17/Nov/2026, 11:01:15 am (UTC)'!
evaluateMethod: method to: receiver logged: doLog profiled: doProfile

	| value toLog itsSelection itsSelectionString |
	
	"Log to .changes is done before the action."
	doLog ifTrue: [
		toLog := ((requestor respondsTo: #selection)  
			and: [ (itsSelection := requestor selection) notNil
			and: [ (itsSelectionString := itsSelection asPlainString) isEmptyOrNil not ]])
				ifTrue: [ itsSelectionString ]
				ifFalse: [ sourceStream contents ].
		Smalltalk logChange: toLog ].

	"Evaluate now."
	doProfile
		ifTrue: [
			TimeProfileBrowser onBlock: [
				value := receiver
					withArgs: (context ifNil: [#()] ifNotNil: [{context}])
					executeMethod: method ]]
		ifFalse: [
			value := receiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method ].

	"Notifications are triggered after the action."
	doLog ifTrue: [
		SystemChangeNotifier uniqueInstance evaluated: toLog context: context ].

	^ value! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 17/Nov/2026, 11:05:44 am (UTC)'!
removeSelector: selector 
	| priorMethod priorProtocol | 
	"Remove the message whose selector is given from the method 
	dictionary of the receiver, if it is there."

	priorMethod := self compiledMethodAt: selector ifAbsent: [^ self].
	(self theNonMetaClass isOkToRemoveMethod: selector isMeta: self isMeta)
		ifFalse: [self error: 'Method removal not allowed'].
	priorProtocol := self whichCategoryIncludesSelector: selector.
	
	"Log to .changes is done before the action."
	Smalltalk logMethodRemoval: priorMethod selector: selector class: self.
	CodeHistory current addMethodRemoval: priorMethod.
	
	SystemChangeNotifier uniqueInstance doSilently: [
		self organization removeElement: selector ].
	super removeSelector: selector.
	
	"Notifications are triggered after the action."
	SystemChangeNotifier uniqueInstance 
			methodRemoved: priorMethod selector: selector inProtocol: priorProtocol class: self.! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 17/Nov/2026, 11:05:52 am (UTC)'!
writeToChangesFile: sourceStr class: class category: catName withStamp: changeStamp oldMethodOrNil: existingMethod

	"Store the source code for the receiver on the external .changes file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	Store a 4-byte source code pointer at the method end."

	| fileIndex file position libIndex |

	Smalltalk userChangesAppendDo: [ :stream |
		self writeMethodPreambleTo: stream class: class category: catName withStamp: changeStamp.
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].

	fileIndex := 2.
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		self writeMethodPreambleTo: file class: class category: catName withStamp: changeStamp.
		file padSpacesTo: SourceFiles pointerScaleForWriting.
		position := file position.
		file nextChunkPut: sourceStr asPlainString.
		file nextChunkPut: ' '.
		"If we are doing a FileIn, don't flush for each method.
		This is a performance optimization, to make file in of large chunks of code faster.
		It means that the source code and the time stamp (and rest of preamble) won't
		be available for reading until the file in finishes.
		This is usually not a problem."
		InMidstOfFileinNotification signal ifFalse: [file flush].
		
		libIndex := 16rFFFFFFFE . "Changes"
		CodeHistory current addMethodVersion: self libraryIndex: libIndex positionIndex: position oldVersionOrNil: existingMethod.
		
		SourceFiles checkOKToAdd: sourceStr size at: position in: fileIndex ].! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 17/Nov/2026, 10:59:53 am (UTC)'!
forgetClass: aClass logged: aBool
	"Delete the class, aClass, from the system.
	Note that this doesn't do everything required to dispose of a class - to do that use Class>>removeFromSystem."

	| classCategory |
	
	"I have to keep the cateogory becuase it is nil after removing the class -Hernan"
	classCategory := aClass category.
	
	"Log to .changes is done before the action."
	aBool ifTrue: [Smalltalk logClassRemoval: aClass].
	
	SystemOrganization removeElement: aClass name.
	
	"Notifications are triggered after the action."
	aBool ifTrue: [SystemChangeNotifier uniqueInstance classRemoved: aClass fromCategory: classCategory].		
	self removeFromStartUpList: aClass.
	self removeFromShutDownList: aClass.
	self removeKey: aClass name ifAbsent: nil.
	self flushClassNameCache! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 17/Nov/2026, 10:50:11 am (UTC)'!
classRemoved: aClass fromCategory: aCategoryName

	| classDefinition |
true ifTrue: [ ^self ].
	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			"I have to recreate the category because the classs has already been removed form the
			SystemOrganizer - Hernan"
			classDefinition := aClass definitionReplacingCategoryWith: aCategoryName. 
			
			self
				logPreamble: 'classRemoval: ', aClass name printString, Utilities changeStampField
				change: classDefinition ].! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 17/Nov/2026, 10:50:16 am (UTC)'!
evaluated: expression context: aContext
true ifTrue: [ ^self ].
	self logChange: expression! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 17/Nov/2026, 10:50:19 am (UTC)'!
methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass
true ifTrue: [ ^self ].
	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			self
				logPreamble: 'methodRemoval: ', aClass name, ' ', aSymbol storeString, Utilities changeStampField
				change: aMethod sourceCode ].! !

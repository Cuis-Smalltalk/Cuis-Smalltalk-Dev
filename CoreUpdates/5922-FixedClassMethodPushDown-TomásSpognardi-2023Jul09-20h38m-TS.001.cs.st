'From Cuis 6.0 [latest update: #5832] on 9 July 2023 at 8:52:36 pm'!

!Metaclass methodsFor: 'class hierarchy' stamp: 'TS 7/9/2023 20:46:15'!
hasSubclasses

	^ thisClass hasSubclasses! !


!Metaclass reorganize!
('accessing' allInstances category isMeta name soleInstance theMetaClass theNonMetaClass)
('copying' postCopy)
('instance creation' new)
('instance variables' addInstVarName: removeInstVarName:)
('pool variables' classPool)
('class hierarchy' addObsoleteSubclass: addSubclass: hasSubclasses obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('compiling' acceptsLoggingOfCompilation bindingOf: localBindingOf: possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: nonTrivial objectForDataStream: storeDataOn:)
('testing' canZapMethodDictionary isObsolete)
('enumerating' allInstancesDo:)
('private' replaceObsoleteInstanceWith:)
('initialization' adoptInstance:from: instanceVariableNames:)
('tdd' testCaseClass)
('accessing instances and variables' allClassVarNames)
!


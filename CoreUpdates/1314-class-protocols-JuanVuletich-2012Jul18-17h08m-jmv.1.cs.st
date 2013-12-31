'From Cuis 4.0 of 21 April 2012 [latest update: #5315] on 19 July 2012 at 10:12:52 am'!

!Metaclass methodsFor: 'instance protocol testing' stamp: 'jmv 7/19/2012 10:12'!
protocols
	"So far, metaclasses don't define protocols. This means, for instance, that
		SmallInteger is: #SomeProtocol
	can not answer anything but false.
	
	It is possible to complete it, maybe adding a new class instance variable to Object to store classProtocols
	"
	^#()! !


!Metaclass reorganize!
('initialize-release' adoptInstance:from: instanceVariableNames:)
('accessing' allInstances category isMeta name soleInstance theMetaClass theNonMetaClass)
('copying' postCopy)
('instance creation' new)
('instance variables' addInstVarName: removeInstVarName:)
('pool variables' classPool)
('class hierarchy' addObsoleteSubclass: addSubclass: obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('compiling' acceptsLoggingOfCompilation bindingOf: possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: nonTrivial objectForDataStream: storeDataOn:)
('testing' canZapMethodDictionary isObsolete)
('enumerating' allInstancesDo:)
('private' replaceObsoleteInstanceWith:)
('instance protocol testing' protocols)
!


'From Cuis 6.0 [latest update: #6068] on 9 November 2023 at 5:24:47 pm'!
!classDefinition: #SystemDictionary category: #'System-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments '
	classVariableNames: 'ChangesInitialFileSize EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp WordSize '
	poolDictionaries: ''
	category: 'System-Support'!

!SystemDictionary methodsFor: 'image' stamp: 'jmv 11/9/2023 17:23:56'!
wordSize
	"Answer the size in bytes of an object pointer or word in the object memory.
	Smalltalk wordSize
	"

	^self vmParameterAt: 40! !

!classDefinition: #SystemDictionary category: #'System-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments'
	classVariableNames: 'ChangesInitialFileSize EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp'
	poolDictionaries: ''
	category: 'System-Support'!

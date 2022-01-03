'From Cuis 5.0 [latest update: #5007] on 30 December 2021 at 4:36:42 pm'!

!RenameClass methodsFor: 'applying - private' stamp: 'HAW 12/30/2021 16:28:15'!
references: aMethodReference classVarNamed: aName

	^(aMethodReference actualClass theNonMetaClass definesClassVariableNamedInHierarchy: aName) 
		or: [ aMethodReference actualClass theMetaClass definesClassVariableNamedInHierarchy: aName ]! !


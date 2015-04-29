'From Cuis 4.2 of 25 July 2013 [latest update: #2257] on 19 April 2015 at 8:56:02.557912 am'!

!ClassBuilder class methodsFor: 'cached state access' stamp: 'jmv 4/19/2015 08:55'!
releaseCachedState
	QuietMode _ nil! !


!ClassBuilder class methodsFor: 'accessing' stamp: 'jmv 4/19/2015 08:54'!
isSilent
	QuietMode ifNil: [
		QuietMode _ false ].
	^QuietMode! !


!ClassBuilder class reorganize!
('accessing' beSilent: beSilentDuring: isSilent)
('cleanup obsolete classes' checkClassHierarchyConsistency checkClassHierarchyConsistency: checkClassHierarchyConsistencyFor: cleanupAndCheckClassHierarchy cleanupAndCheckClassHierarchy: cleanupClassHierarchy cleanupClassHierarchy: cleanupClassHierarchyFor: countReallyObsoleteClassesAndMetaclasses isReallyObsolete: reallyObsoleteClasses reallyObsoleteMetaclasses)
('cached state access' releaseCachedState)
!


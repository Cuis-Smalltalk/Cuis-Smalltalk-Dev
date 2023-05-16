'From Cuis 6.0 [latest update: #5799] on 11 May 2023 at 1:03:45 pm'!

!ClassBuilder class methodsFor: 'cleanup obsolete classes' stamp: 'jmv 5/11/2023 13:02:49'!
checkClassHierarchyConsistency
	"Check the consistency of the class hierarchy. The class hierarchy is consistent if the following
	two logical equivalences hold for classes A and B:
	- B is obsolete and 'B superclass' yields A  <-->  'A obsoleteSubclasses' contains B
	- B is not obsolete and 'B superclass' yields A  <-->  'A subclasses' contains B
	ClassBuilder checkClassHierarchyConsistency
	"
	Utilities informUserDuring: [ :bar |
		self checkClassHierarchyConsistency: bar.
	].! !

!ClassBuilder class methodsFor: 'cleanup obsolete classes' stamp: 'jmv 5/11/2023 13:03:24'!
cleanupAndCheckClassHierarchy
	"Makes the class hierarchy consistent and removes obsolete classes from the SystemDictionary.
	Afterwards it checks whether the hierarchy is really consistent.
	ClassBuilder cleanupAndCheckClassHierarchy
	"
	Utilities informUserDuring: [ :bar |
		self cleanupAndCheckClassHierarchy: bar.
	].
! !


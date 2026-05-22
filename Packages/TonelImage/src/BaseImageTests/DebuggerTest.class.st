Class {
	#name : 'DebuggerTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Tools-Debugger',
	#package : 'BaseImageTests',
	#tag : 'Tools-Debugger'
}

{ #category : 'tests - method categories' }
DebuggerTest >> testCategoriesAreAddedSortedByAndSeparatedByClass [

	| objectCategories protoObjectCategories categoriesPrompter |

	objectCategories := Object methodCategoriesAsSortedCollection.
	protoObjectCategories := ProtoObject methodCategoriesAsSortedCollection removeAllFoundIn: objectCategories; yourself.

	"Test pre-conditions. May seem unnecesary but they are not if we take time into account - Hernan"
	self assert: Object superclass equals: ProtoObject.
	self assert: ProtoObject superclass equals: nil.
	self assert: objectCategories notEmpty.
	self assert: protoObjectCategories notEmpty.

	categoriesPrompter := MethodCategoriesPrompter startingFrom: Object rejectingFirst: false.

	self assert: categoriesPrompter lines size equals: 2.
	self assert: categoriesPrompter lines first equals: 1.
	self assert: (categoriesPrompter categories copyFrom: 2 to: categoriesPrompter lines second) asArray equals: objectCategories asArray.
	self assert: (categoriesPrompter categories copyFrom: categoriesPrompter lines second + 1 to: categoriesPrompter categories size) asArray equals: protoObjectCategories asArray.
]

{ #category : 'tests - method categories' }
DebuggerTest >> testMetaclassCategoriesIncludesInstanceCreation [

	| categoriesPrompter |

	categoriesPrompter := MethodCategoriesPrompter startingFrom: ProtoObject class rejectingFirst: false.

	self assert: categoriesPrompter categories second equals: Categorizer instanceCreation.
]

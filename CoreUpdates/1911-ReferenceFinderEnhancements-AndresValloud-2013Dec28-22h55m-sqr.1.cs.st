'From Cuis 4.2 of 25 July 2013 [latest update: #1910] on 28 December 2013 at 11:00:23.271901 pm'!
!classDefinition: #ReferencesExplorerWrapper category: #'Tools-ReferenceFinder'!
ObjectExplorerWrapper subclass: #ReferencesExplorerWrapper
	instanceVariableNames: 'finderSkipsClasses '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!
!classDefinition: #ReferenceFinderMirror category: #'Tools-ReferenceFinder'!
Object subclass: #ReferenceFinderMirror
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

!ReferenceFinderMirror commentStamp: '<historical>' prior: 0!
This class exists solely to implement the mirror primitives in a more accessible place.!


!ReferencesExplorerWrapper methodsFor: 'printing' stamp: 'sqr 12/28/2013 22:24'!
pathStringFrom: parentObject to: childObject count: anInteger

	| answer referenceDescription |
	answer _ (String new: 32) writeStream.
	referenceDescription _ self referenceDescriptionFrom: parentObject to: childObject count: anInteger.
	self print: parentObject on: answer.
	answer
		nextPutAll: ' ';
		nextPutAll: referenceDescription;
		nextPutAll: ' --> '.
	self print: childObject on: answer.
	^answer contents! !

!ReferencesExplorerWrapper methodsFor: 'printing' stamp: 'sqr 12/28/2013 22:44'!
referenceDescriptionFrom: parentObject to: childObject count: anInteger

	| parentClass referencesSoFar |
	referencesSoFar _ 0.
	parentClass _ ReferenceFinderMirror objectClass: parentObject.
	(self finderSkipsClasses not and: [parentClass == childObject]) ifTrue:
		[
			referencesSoFar _ referencesSoFar + 1.
			referencesSoFar = anInteger ifTrue: [^'class']
		].
	parentClass == CompiledMethod
		ifTrue:
			[
				1 to: parentObject numLiterals do:
					[:eachIndex |
						| eachLiteral |
						eachLiteral _ parentObject literalAt: eachIndex.
						eachLiteral == childObject ifTrue:
							[
								referencesSoFar _ referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^'literalAt: ', eachIndex printString]
							]
					]
			]
		ifFalse:
			[
				1 to: parentClass instSize do:
					[:eachIndex |
						| eachVariable |
						eachVariable _ ReferenceFinderMirror object: parentObject instVarAt: eachIndex.
						eachVariable == childObject ifTrue:
							[
								referencesSoFar _ referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^parentClass allInstVarNames at: eachIndex]
							]
					].
				1 to: (ReferenceFinderMirror objectSize: parentObject) do:
					[:eachIndex |
						| eachSlot |
						eachSlot _ ReferenceFinderMirror object: parentObject basicAt: eachIndex.
						eachSlot == childObject ifTrue:
							[
								referencesSoFar _ referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^'at: ', eachIndex printString]
							]
					]
			].
	^'unknown (maybe the reference changed)'! !

!ReferencesExplorerWrapper methodsFor: 'accessing' stamp: 'sqr 12/28/2013 22:33'!
finderSkipsClasses

	^finderSkipsClasses! !

!ReferencesExplorerWrapper methodsFor: 'accessing' stamp: 'sqr 12/28/2013 22:33'!
finderSkipsClasses: aBoolean

	finderSkipsClasses _ aBoolean! !


!ReferenceFinder methodsFor: 'accessing' stamp: 'sqr 12/28/2013 22:32'!
skipClasses
	"For the time being"

	^true! !


!ReferenceFinderMirror class methodsFor: 'mirror primitives' stamp: 'sqr 12/28/2013 22:42'!
object: anObject basicAt: index 
	"Answer the value of an indexable element in the argument anObject without sending
	 it a message. Fail if the argument index is not an Integer or is out of bounds, or if
	 anObject is not indexable. This mimics the action of the VM when it indexes an object.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self object: anObject basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]! !

!ReferenceFinderMirror class methodsFor: 'mirror primitives' stamp: 'sqr 12/28/2013 22:49'!
object: anObject instVarAt: anIndex
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	 variables corresponds to the named instance variables. Fail if the index 
	 is not an Integer or is not the index of a fixed variable. Essential for the
	 debugger. See  Object documentation whatIsAPrimitive."

	<primitive: 73>
	"Access beyond fixed variables."
	^self object: anObject basicAt: anIndex - (self objectClass: anObject) instSize! !

!ReferenceFinderMirror class methodsFor: 'mirror primitives' stamp: 'sqr 12/28/2013 22:45'!
objectClass: anObject

	<primitive: 111>
	self primitiveFailed! !

!ReferenceFinderMirror class methodsFor: 'mirror primitives' stamp: 'sqr 12/28/2013 22:45'!
objectSize: anObject
	"Answer the number of indexable variables in the argument anObject without sending
	 it a message. This mimics the action of the VM when it fetches an object's variable size.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0! !


!ReferencesExplorer methodsFor: 'references' stamp: 'sqr 12/28/2013 22:33'!
getList

	| wrapper |
	wrapper _ ReferencesExplorerWrapper
		with: self finder rootObject
		name: self rootObject identityHash asString
		model: self finder rootNode.
	wrapper finderSkipsClasses: self finder skipClasses.
	^Array with: wrapper! !


!ReferencesExplorerWrapper methodsFor: 'reference chain' stamp: 'sqr 12/28/2013 22:34'!
contents

	| answer |
	answer _ Array new: model paths size.
	model paths doWithIndex:
		[:each :eachIndex |
			| name newWrapper |
			name _ self pathStringFrom: model object to: each object count: eachIndex.
			newWrapper _ self class with: each object name: name model: each parent: self.
			newWrapper finderSkipsClasses: self finderSkipsClasses.
			answer at: eachIndex put: newWrapper.
		].
	^answer! !


!ReferenceFinder methodsFor: 'private' stamp: 'sqr 12/28/2013 22:45'!
newRootNode

	| rootObject rootObjectClass node |
	rootObject _ self rootObject.
	rootObjectClass _ ReferenceFinderMirror objectClass: rootObject.
	node _ CompiledMethod == rootObjectClass
		ifTrue: [ReferenceTraceCompiledMethodNode object: rootObject]
		ifFalse: [ReferenceTraceNode object: rootObject].
	node classOfObject: rootObjectClass.
	^node! !

!ReferenceFinder methodsFor: 'private - references' stamp: 'sqr 12/28/2013 22:47'!
shouldTraceInto: anObject instanceOf: aClass
	"Tracing assumes UndefinedObjects do not have referents.  Also, note
	that sending isBits is faster than selecting the bits classes and doing linear
	search by a factor of about 3x.  The SmallInteger is specialized because
	overall it achieves 5% performance gain.  Finally, assume aClass is not
	CompiledMethod"

	nil == anObject ifTrue: [^false].
	(SmallInteger == aClass or: [aClass isBits]) ifTrue: [^false].
	((ReferenceFinderMirror objectSize: anObject) = 0
		and: [aClass instSize = 0])
			ifTrue: [^false].
	(self skipWeakObjects and: [aClass isWeak]) ifTrue: [^false].
	(self nodes includes: anObject) ifTrue: [^false].
	self nodes add: anObject.
	^true! !


!ReferenceTraceNode methodsFor: 'references' stamp: 'sqr 12/28/2013 22:43'!
breadthFirstFindReferencesTo: anObject for: aFinder

	1 to: self classOfObject instSize do:
		[:each |
			self
				breadthFirstReferencesTo: anObject
				traceReferent: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				breadthFirstReferencesTo: anObject
				traceReferent: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]! !

!ReferenceTraceNode methodsFor: 'references' stamp: 'sqr 12/28/2013 22:43'!
depthFirstFindReferencesTo: anObject for: aFinder

	1 to: self classOfObject instSize do:
		[:each |
			self
				depthFirstReferencesTo: anObject
				traceReferent: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				depthFirstReferencesTo: anObject
				traceReferent: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/28/2013 22:46'!
breadthFirstReferencesTo: aTarget traceReferent: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldTraceIntoCompiledMethod: anObject) ifFalse: [^self].
				node _ ReferenceTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldTraceInto: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ ReferenceTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	aFinder scanQueueAdd: node! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/28/2013 22:46'!
depthFirstReferencesTo: aTarget traceReferent: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldTraceIntoCompiledMethod: anObject) ifFalse: [^self].
				node _ ReferenceTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldTraceInto: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ ReferenceTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	node depthFirstFindReferencesTo: aTarget for: aFinder! !

!ReferenceTraceNode methodsFor: 'private - references' stamp: 'sqr 12/28/2013 22:46'!
path: x comesBefore: y

	x object identityHash < y object identityHash ifTrue: [^true].
	x object identityHash > y object identityHash ifTrue: [^false].
	^(ReferenceFinderMirror objectClass: x) name
		<= (ReferenceFinderMirror objectClass: y) name! !

!methodRemoval: ReferenceTraceNode #object:basicAt:!
ReferenceTraceNode removeSelector: #object:basicAt:!
!methodRemoval: ReferenceTraceNode #object:instVarAt:!
ReferenceTraceNode removeSelector: #object:instVarAt:!
!methodRemoval: ReferenceTraceNode #objectClass:!
ReferenceTraceNode removeSelector: #objectClass:!
!methodRemoval: ReferenceTraceNode #objectSize:!
ReferenceTraceNode removeSelector: #objectSize:!
!methodRemoval: ReferenceFinder #objectClass:!
ReferenceFinder removeSelector: #objectClass:!
!methodRemoval: ReferenceFinder #objectSize:!
ReferenceFinder removeSelector: #objectSize:!
!methodRemoval: ReferencesExplorerWrapper #pathStringTo:!
ReferencesExplorerWrapper removeSelector: #pathStringTo:!
!classDefinition: #ReferencesExplorerWrapper category: #'Tools-ReferenceFinder'!
ObjectExplorerWrapper subclass: #ReferencesExplorerWrapper
	instanceVariableNames: 'finderSkipsClasses'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-ReferenceFinder'!

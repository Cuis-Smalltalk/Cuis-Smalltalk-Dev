'From Cuis 5.0 of 7 November 2016 [latest update: #3115] on 20 June 2017 at 5:50:34 pm'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 6/20/2017 13:47:09'!
browseObsoleteMethodReferences
	"Open a browser on all referenced behaviors that are obsolete
		Smalltalk browseObsoleteMethodReferences
	Remember that if no methods reference obsoletes, but
		Smalltalk obsoleteBehaviors inspect
	still finds them, maybe they are referenced by ChangeSets!!
	"
	| list |
	list _ self obsoleteMethodReferences.
	self browseMessageList: list name:'Method referencing obsoletes' autoSelect: nil! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 6/20/2017 13:46:17'!
obsoleteBehaviors
	"
	Smalltalk obsoleteBehaviors inspect
	Find all obsolete behaviors including meta classes
	"
	| obs |
	obs _ OrderedCollection new.
	Smalltalk garbageCollect.
	self allObjectsDo: [ :cl |
		(cl isBehavior and: [cl isObsolete]) ifTrue: [obs add: cl]].
	^ obs asArray! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 6/20/2017 13:46:10'!
obsoleteMethodReferences
	"
	Smalltalk obsoleteMethodReferences
	Smalltalk browseObsoleteMethodReferences
	Open a browser on all referenced behaviors that are obsolete"
	| obsClasses obsRefs references |
	references _ WriteStream on: Array new.
	obsClasses _ self obsoleteBehaviors.
	'Scanning for methods referencing obsolete classes' displayProgressAt: Sensor mousePoint
		from: 1 to: obsClasses size during: [ :bar |
	obsClasses keysAndValuesDo: [ :index :each |
		bar value: index.
		obsRefs _ self pointersTo: each except: obsClasses.
		obsRefs do: [ :ref |
			"Figure out if it may be a global"
			(ref isVariableBinding and: [ ref key isString "or Symbol" ]) ifTrue: [
				(self pointersTo: ref) do: [ :meth |
					(meth is: #CompiledMethod) ifTrue: [
						meth methodReference ifNotNil: [ :mref |
							(mref isValid and: [ mref compiledMethod == meth]) ifTrue: [
								references nextPut: mref ]]]]]]].
	].
	^references contents! !


!MethodReference methodsFor: 'queries' stamp: 'jmv 6/20/2017 13:30:02'!
printOn: aStream
	"Print the receiver on a stream"

	super printOn: aStream.
	aStream
		space;
		nextPutAll: classSymbol.
	classIsMeta ifTrue: [ aStream nextPutAll: ' class' ].
	aStream
		nextPutAll: ' >> ';
		nextPutAll: methodSymbol! !


'From Cuis 4.2 of 25 July 2013 [latest update: #2180] on 7 February 2015 at 10:20:23.510551 pm'!
!classDefinition: #QSystemTally category: #'Tools-Profiling'!
Link subclass: #QSystemTally
	instanceVariableNames: 'class method process receivers tally blockNesting '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/7/2015 22:12'!
blockNestingCountOf: context
	| count nest |
	count := 0.
	nest := context.
	[nest closure notNil] whileTrue:
		[count := count + 1.
		 nest := nest closure outerContext].
	^count! !

!QSystemTally methodsFor: 'accessing' stamp: 'jmv 2/7/2015 22:14'!
blockNestingCount
	^blockNesting! !

!QSystemTally methodsFor: 'initialization' stamp: 'jmv 2/7/2015 22:12'!
class: aClass method: aCompiledMethod nesting: blockNestingCount
	class := aClass.
	method := aCompiledMethod.
	blockNesting := blockNestingCount.
	tally := 0! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 2/7/2015 22:11'!
printOn: aStream
	blockNesting timesRepeat: [
		aStream nextPutAll: '[] in '].
	aStream print: class; nextPutAll: '>>'; print: (method ifNotNil:[method selector]).
	aStream nextPutAll: ' -- '; print: tally.! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 2/7/2015 22:16'!
printOn: aStream total: total totalTime: totalTime

	| className aSelector aClass percentage |
	percentage := tally asFloat / total * 100.0.
	percentage printOn: aStream fractionDigits: 1.
	aStream
		nextPutAll: '% (';
		nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
		nextPutAll: ' ms) '.

	aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
	className := aClass name contractTo: self maxClassNameSize.
			blockNesting > 0 ifTrue:
				[aStream
					next: blockNesting put: $[;
					next: blockNesting put: $];
					space].
	aStream
		nextPutAll: class name;
		nextPutAll: (aClass = class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
		nextPutAll: (aSelector contractTo: self maxClassPlusSelectorSize - className size);
		newLine ! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/7/2015 22:14'!
tallyLeafChild: context inProcess: aProcess by: count
	"Find a suitable child, or add a new one if needed."
	| aMethod node nesting |
	aMethod := context method.
	nesting := self blockNestingCountOf: context.
	node _ receivers.
	[node == nil] whileFalse: [
		(node method == aMethod and: [ node process == aProcess and: [ node blockNestingCount = nesting]]) ifTrue: [
			^node addTallies: count ].
		node _ node nextLink ].
	"Entender en que caso pudo fallar arriba por el proceso, y agregarse aca.
	O sea, en que caso habra 2 nodos hermanos iguales salvo el proceso.
	mhhhhhh"
	node _ QSystemTally new
				class: context receiver class
				method: aMethod
				nesting: nesting.
	node process: aProcess.
	node nextLink: receivers.
	receivers _ node.
	^node addTallies: count! !

!QSystemTally methodsFor: 'initialization' stamp: 'jmv 2/7/2015 22:11'!
class: aClass method: aCompiledMethod
	class := aClass.
	method := aCompiledMethod.
	tally := blockNesting := 0! !

!classDefinition: #QSystemTally category: #'Tools-Profiling'!
Link subclass: #QSystemTally
	instanceVariableNames: 'class method process receivers tally blockNesting'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

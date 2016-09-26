'From Cuis 4.2 of 25 July 2013 [latest update: #2943] on 20 September 2016 at 7:29:47 pm'!

!DecompilerConstructor commentStamp: '<historical>' prior: 0!
I construct the node tree for a Decompiler.!


!DecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 9/20/2016 19:26:00'!
codeRemoteTemp: index remoteTemps: tempVector

	^(RemoteTempVectorNode new
		name: 'remoteVar', index printString
		index: index
		type: LdTempType
		scope: 0)
			remoteTemps: tempVector;
			yourself! !


!Decompiler methodsFor: 'private' stamp: 'jmv 9/20/2016 19:27:56'!
constructorForMethod: aMethod
	^DecompilerConstructor new! !


!DecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 9/20/2016 19:27:04'!
codeMethod: selector block: block tempVars: vars primitive: primitive class: class

	| blockNode selectorNode visibleTemps invisibleTemps arguments temporaries properties |
	selectorNode := self codeSelector: selector code: nil.
	tempVars := vars.
	visibleTemps := OrderedCollection new.
	invisibleTemps := OrderedCollection new.
	tempVars do: [:t|
				   ((t isIndirectTempVector or: [t scope >= 0])
						ifTrue: [visibleTemps]
						ifFalse: [invisibleTemps]) addLast: t].
	arguments := visibleTemps copyFrom: 1 to: nArgs.
	temporaries := visibleTemps copyFrom: nArgs + 1 to: visibleTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	properties := method properties copy.
	(properties at: #onceCache ifAbsent: []) ifNotNil:
		[:onceCache|
		 properties := properties copyWithout: (Association
													key: #onceCache
													value: onceCache)].
	blockNode := MethodNode new
		selector: selectorNode
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (method encoderClass new initScopeAndLiteralTables
					temps: visibleTemps, invisibleTemps
					literals: literalValues
					class: class)
		primitive: primitive
		properties: properties.
	blockNode properties method: blockNode.
	^blockNode! !

!classRemoval: #DecompilerConstructorForClosures!
Smalltalk removeClassNamed: #DecompilerConstructorForClosures!

'From Cuis 5.0 [latest update: #3887] on 26 September 2019 at 7:38:16 pm'!
!classDefinition: #SerializableClosureDecompiler category: #'System-Support'!
Decompiler subclass: #SerializableClosureDecompiler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!classDefinition: #SerializableClosureAssignmentNode category: #'System-Support'!
AssignmentNode subclass: #SerializableClosureAssignmentNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!classDefinition: #SerializableClosureInstanceVariableNode category: #'System-Support'!
InstanceVariableNode subclass: #SerializableClosureInstanceVariableNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!classDefinition: #SerializableClosureTempVariableNode category: #'System-Support'!
TempVariableNode subclass: #SerializableClosureTempVariableNode
	instanceVariableNames: 'capturedIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!classDefinition: #SerializableClosureDecompilerConstructor category: #'System-Support'!
DecompilerConstructor subclass: #SerializableClosureDecompilerConstructor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!classDefinition: #SerializableBlockClosure category: #'System-Support'!
Object subclass: #SerializableBlockClosure
	instanceVariableNames: 'theSelf sourceCode capturedValues'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!SerializableBlockClosure commentStamp: 'jmv 9/25/2019 17:21:45' prior: 0!
A SerializableBlockClosure is a regular Smalltalk object (and hence serializable by ReferenceStream and SmartRefStream), that hold the source code for a BlockClosure, together with the state (originally in outer temps) that the BlockClosure might access.

When sent #asEvaluable (even after materializing in the same or different Smalltalk image / machine), the result is a BlockClosure that can be evaluated with identical result as the original.

By making BlockClosure>>objectForDataStream: call #asSerializable, we enable serialization and #veryDeepCopy of BlockClosures as if they were regular Smalltalk objects.

(The only limitation is that we can't meaningfully handle non-local returns. This limitation also applies to regular BlockClosures if evaluation is attempted when there's nowhere to return to.)!


!BlockClosure methodsFor: 'objects from disk' stamp: 'jmv 9/20/2019 21:17:34'!
asSerializable
	^SerializableBlockClosure onBlockClosure: self! !


!SerializableClosureDecompiler methodsFor: 'instruction decoding' stamp: 'jmv 9/25/2019 18:23:40'!
pushReceiver

	stack addLast: (InstanceVariableNode new
		name: 'theSelf'
		index: 1)! !

!SerializableClosureDecompiler methodsFor: 'public access' stamp: 'jmv 9/25/2019 18:00:20'!
decompileBlockAndMethod: aBlock
	"Decompile aBlock, returning the result as a BlockNode, and the corresponding MethodNode."
	"Decompiler new decompileBlockAndMethod: [3 + 4]"

	| homeMethod methodNode home methodClass methodSelector |
	(home := aBlock home) ifNil: [^ nil].
	homeMethod := home method.
	(home methodClass) == #unknown ifTrue: [^ nil].
	aBlock isClosure ifTrue: [
		methodClass := homeMethod methodClass ifNil: [Object].
		methodSelector := homeMethod selector ifNil: [homeMethod defaultSelector].
		methodNode := self decompile: methodSelector in: methodClass method: homeMethod.
		methodNode
			ifNil: [^nil]
			ifNotNil: [methodNode nodesDo: [:node| node pc = aBlock startpc ifTrue: [^{node. methodNode}]]].
		 ^self error: 'cannot find block node matching aBlock'].
	^self error: 'can only decompile BlockClosures'! !

!SerializableClosureDecompiler methodsFor: 'private' stamp: 'jmv 9/26/2019 18:48:55'!
constructorForMethod: aMethod

	^SerializableClosureDecompilerConstructor new! !


!SerializableClosureAssignmentNode methodsFor: 'printing' stamp: 'jmv 9/26/2019 18:47:12'!
printOn: aStream indent: level

	"For temps and args local to a block"
	(variable isTemp and: [variable isCapturedByClosure not]) ifTrue: [ ^super printOn: aStream indent: level ].

	"For outer temps, but also for ivars"
	aStream nextPutAll: '('.
	variable printIndirectOn: aStream indent: level.
	aStream nextPutAll: ' put: '.
	value printOn: aStream indent: level.
	aStream nextPutAll: ')'.! !


!SerializableClosureInstanceVariableNode methodsFor: 'printing' stamp: 'jmv 9/26/2019 18:44:09'!
printIndirectOn: aStream indent: level

	aStream nextPutAll: 'theSelf instVarNamed: ''', name, ''''! !

!SerializableClosureInstanceVariableNode methodsFor: 'printing' stamp: 'jmv 9/26/2019 18:44:22'!
printOn: aStream indent: level 

	aStream nextPut: $(.
	self printIndirectOn: aStream indent: level.
	aStream nextPut: $).! !


!SerializableClosureTempVariableNode methodsFor: 'printing' stamp: 'jmv 9/26/2019 18:44:13'!
printIndirectOn: aStream indent: level 
	
	self isRemote
		ifTrue: [ aStream nextPutAll: 'capturedValues last at: ', capturedIndex printString ]
		ifFalse: [ aStream nextPutAll: 'capturedValues at: ', capturedIndex printString ]! !

!SerializableClosureTempVariableNode methodsFor: 'printing' stamp: 'jmv 9/26/2019 18:47:08'!
printOn: aStream indent: level

	"For temps local to the block"
	self isCapturedByClosure ifFalse: [
		^super printOn: aStream indent: level ].

	"For outer temps"
	aStream nextPut: $(.
	self printIndirectOn: aStream indent: level.
	aStream nextPut: $).! !

!SerializableClosureTempVariableNode methodsFor: 'testing' stamp: 'jmv 9/26/2019 16:14:58'!
isCapturedByClosure

	^capturedIndex notNil! !

!SerializableClosureTempVariableNode methodsFor: 'accessing' stamp: 'jmv 9/26/2019 16:03:48'!
capturedIndex: idx

	capturedIndex _ idx! !


!SerializableClosureDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 9/26/2019 18:49:23'!
codeAssignTo: variable value: expression

	^ SerializableClosureAssignmentNode new variable: variable value: expression! !

!SerializableClosureDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 9/26/2019 18:49:54'!
codeInst: index

	^ SerializableClosureInstanceVariableNode new
		name: (instVars at: index + 1 ifAbsent: ['unknown', index asString])
		index: index + 1! !

!SerializableClosureDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 9/26/2019 18:49:40'!
codeTemp: index

	^ SerializableClosureTempVariableNode new
		name: 'temp' , (index + 1) printString
		index: index
		type: LdTempType
		scope: 0! !

!SerializableClosureDecompilerConstructor methodsFor: 'constructor' stamp: 'jmv 9/26/2019 18:49:40'!
codeTemp: index named: tempName

	^ SerializableClosureTempVariableNode new
		name: tempName
		index: index
		type: LdTempType
		scope: 0! !


!SerializableBlockClosure methodsFor: 'initialization' stamp: 'jmv 9/26/2019 19:34:11'!
onBlockClosure: aBlockClosure

	| both blockNode methodNode indirectTempNames sortedOuterTemps ownNames usedOuterNames sortedUsedOuterNames |
	aBlockClosure hasNonLocalReturn ifTrue: [
		self error: 'Can not serialize closures with non-local returns.' ].
	aBlockClosure sendsToSuper ifTrue: [
		self error: 'Can not currently serialize closures with super sends.' ].
	both _ SerializableClosureDecompiler new  decompileBlockAndMethod: aBlockClosure.
	blockNode _ both first.
	methodNode _ both second.

	indirectTempNames _ methodNode temporaries
		detect: [ :node | node isIndirectTempVector ]
		ifFound: [ :node | node remoteTemps collect: [ :n | n name ]]
		ifNone: [#()].
	sortedOuterTemps _ OrderedCollection new.
	aBlockClosure outerContextsDo: [ :c | c closure ifNotNil: [ :cc |
		| ccn |
		ccn _ cc decompile.
		sortedOuterTemps addAll: ccn arguments; addAll: ccn temporaries ]].
	sortedOuterTemps addAll: methodNode temporaries; addAll: methodNode arguments.

	ownNames _ ((blockNode arguments, blockNode temporaries)
		collect: [ :node | node name ]) asSet.
	usedOuterNames _ Set new.
	blockNode nodesDo: [ :node | node isTemp ifTrue: [
		(ownNames includes: node name) | (indirectTempNames includes: node name) ifFalse: [
			usedOuterNames add: node name]]].

	sortedUsedOuterNames _ sortedOuterTemps select: [ :node |
		usedOuterNames includes: node name ]. "sort them"
	sortedUsedOuterNames _ sortedUsedOuterNames collect: [ :node | node name ].

	blockNode nodesDo: [ :node | node isTemp ifTrue: [ 
		node isRemote
			ifTrue: [node capturedIndex: (indirectTempNames indexOf: node name) ]
			ifFalse: [
				(sortedUsedOuterNames includes: node name)
					ifTrue: [node capturedIndex: (sortedUsedOuterNames indexOf: node name)]]]].

	theSelf _ aBlockClosure receiver.
	capturedValues _ aBlockClosure capturedValues.
	sourceCode _ blockNode decompileString.! !

!SerializableBlockClosure methodsFor: 'evaluating' stamp: 'jmv 9/25/2019 17:14:00'!
asEvaluable

	^Compiler evaluate: sourceCode for: self logged: false! !

!SerializableBlockClosure methodsFor: 'objects from disk' stamp: 'jmv 9/25/2019 11:43:02'!
comeFullyUpOnReload: smartRefStream

	^ self asEvaluable! !


!SerializableBlockClosure class methodsFor: 'instance creation' stamp: 'jmv 9/20/2019 21:18:13'!
onBlockClosure: aBlockClosure
	^self new onBlockClosure: aBlockClosure! !


!BlockClosure methodsFor: 'objects from disk' stamp: 'jmv 9/26/2019 09:26:56'!
objectForDataStream: refStrm
	"I am about to be written on an object file.
	Write a textual reference if possible. If not, attempt converting to a serializable object.
	This might also fail. See #onBlockClosure:"

	self isCleanClosure ifTrue: [
		^ DiskProxy
			global: #Compiler
			selector: #evaluate:
			args: (Array with: self decompile decompileString) ].
	^self asSerializable! !


!MethodContext methodsFor: 'accessing' stamp: 'jmv 9/26/2019 17:23:06'!
outerContextsDo: aBlock 
	"Answer the context in which the receiver was defined."

	aBlock value: self.
	closureOrNil
		ifNotNil: [ closureOrNil outerContextsDo: aBlock ].! !


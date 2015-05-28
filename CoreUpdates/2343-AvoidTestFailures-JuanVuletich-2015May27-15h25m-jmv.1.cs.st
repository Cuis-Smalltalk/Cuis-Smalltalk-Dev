'From Cuis 4.2 of 25 July 2013 [latest update: #2342] on 27 May 2015 at 3:27:51.74274 pm'!

!ClassCommentVersionsBrowser methodsFor: 'misc' stamp: 'jmv 5/27/2015 13:23'!
priorSourceOrNil
	"If the currently-selected method has a previous version, return its source, else return nil"
	| changeRecords |
	self selectedClass ifNil: [^ nil].
	self selectedMessageName ifNil: [^ nil].
	changeRecords _  self class commentRecordsOf: self selectedClass.
	(changeRecords == nil or: [changeRecords size <= 1]) ifTrue: [^ nil].
	^ (changeRecords at: 2) string 
! !


!AdditionalMethodState methodsFor: 'testing' stamp: 'jmv 5/27/2015 14:00'!
hasLiteralSuchThat: aBlock
	"Answer true if litBlock returns true for any literal in this array, even if embedded in further array structure.
	 This method is only intended for private use by CompiledMethod hasLiteralSuchThat:"
	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
			ifTrue: [(aBlock value: propertyOrPragma key)
					or: [(aBlock value: propertyOrPragma value)
					or: [propertyOrPragma value isArray
						and: [propertyOrPragma value hasLiteralSuchThat: aBlock]]]]
			ifFalse: [propertyOrPragma hasLiteralSuchThat: aBlock]) ifTrue: [^true]].
	^false! !

!AdditionalMethodState methodsFor: 'testing' stamp: 'jmv 5/27/2015 14:00'!
hasLiteralThorough: literal
	"Answer true if any literal in these properties is literal,
	 even if embedded in array structure."
	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
			ifTrue: [propertyOrPragma key == literal
					or: [propertyOrPragma value == literal
					or: [propertyOrPragma value isArray
						and: [propertyOrPragma value hasLiteral: literal]]]]
			ifFalse: [propertyOrPragma hasLiteral: literal]) ifTrue:
			[^true]].
	^false! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 5/27/2015 13:55'!
at: aKey ifAbsent: aBlock
	"Answer the property value or pragma associated with aKey or,
	 if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do:
		[:i |
		(propertyOrPragma := self basicAt: i) key == aKey ifTrue:
			[^propertyOrPragma isVariableBinding
				ifTrue: [propertyOrPragma value]
				ifFalse: [propertyOrPragma]]].
	^aBlock value! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 5/27/2015 13:55'!
at: aKey ifAbsentPut: aBlock
	"Answer the property value or pragma associated with aKey or,
	 if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do:
		[:i |
		(propertyOrPragma := self basicAt: i) key == aKey ifTrue:
			[^propertyOrPragma isVariableBinding
				ifTrue: [propertyOrPragma value]
				ifFalse: [propertyOrPragma]]].
	^method propertyValueAt: aKey put: aBlock value! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 5/27/2015 13:56'!
at: aKey put: aValue
	"Replace the property value or pragma associated with aKey."

	| keyAlreadyExists propertyOrPragma "<Association|Pragma>" |
	keyAlreadyExists _ false.
	
	1 to: self basicSize do: [ :i |
		(propertyOrPragma _ self basicAt: i) key == aKey ifTrue: [
			keyAlreadyExists _ true.
			propertyOrPragma isVariableBinding
				ifTrue: [ propertyOrPragma value: aValue ]
				ifFalse: [ self basicAt: i put: aValue ]]].
	
	keyAlreadyExists ifFalse: [
		method propertyValueAt: aKey put: aValue ].
	
	^ aValue! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 5/27/2015 13:57'!
keysAndValuesDo: aBlock
	"Enumerate the receiver with all the keys and values."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding
			ifTrue: [aBlock value: propertyOrPragma key value: propertyOrPragma value]
			ifFalse: [aBlock value: propertyOrPragma keyword value: propertyOrPragma]]! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 5/27/2015 13:56'!
pragmas
	"Answer the raw messages comprising my pragmas."
	| pragmaStream propertyOrPragma "<Association|Message>" |
	pragmaStream := WriteStream on: (Array new: self basicSize).
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifFalse:
			[pragmaStream nextPut: propertyOrPragma]].
	^pragmaStream contents! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 5/27/2015 13:55'!
properties

	| propertyStream propertyOrPragma "<Association|Pragma>" |
	propertyStream := WriteStream on: (Array new: self basicSize * 2).
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifTrue:
			[propertyStream nextPut: propertyOrPragma key; nextPut: propertyOrPragma value]].
	^IdentityDictionary newFromPairs: propertyStream contents! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 5/27/2015 13:54'!
removeKey: aKey ifAbsent: aBlock
	"Remove the property with aKey. Answer the value or, if aKey isn't found, answer the result of evaluating aBlock."
	
	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
				ifTrue: [propertyOrPragma key]
				ifFalse: [propertyOrPragma keyword])
			== aKey ifTrue:
			[^method removeProperty: aKey]].
	^aBlock value! !

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 5/27/2015 13:56'!
setMethod: aMethod
	| propertyOrPragma "<Association|Pragma>" |
	method := aMethod.
	1 to: self basicSize do:
		[ :i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifFalse:
			[propertyOrPragma setMethod: aMethod]]! !

!AdditionalMethodState methodsFor: 'properties' stamp: 'jmv 5/27/2015 13:56'!
includesProperty: aKey
	"Test if the property aKey is present."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
		 and: [propertyOrPragma key == aKey]) ifTrue:
			[^true]].
	^false! !

!AdditionalMethodState methodsFor: 'properties' stamp: 'jmv 5/27/2015 14:00'!
propertyKeysAndValuesDo: aBlock
	"Enumerate the receiver with all the keys and values."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifTrue:
			[aBlock value: propertyOrPragma key value: propertyOrPragma value]]! !

!AdditionalMethodState methodsFor: 'properties' stamp: 'jmv 5/27/2015 14:00'!
propertyValueAt: aKey ifAbsent: aBlock
	"Answer the property value associated with aKey or, if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
		 and: [propertyOrPragma key == aKey]) ifTrue:
			[^propertyOrPragma value]].
	^aBlock value! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 5/27/2015 13:27'!
classComment: aString stamp: aStamp
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| ptr header oldCommentRemoteStr |
	(aString isKindOf: RemoteString) ifTrue: [
		SystemChangeNotifier uniqueInstance classCommented: self.
		^ self organization classComment: aString stamp: aStamp].

	oldCommentRemoteStr _ self organization commentRemoteStr.
	(aString size = 0) & (oldCommentRemoteStr == nil) ifTrue: [^ self organization classComment: nil].
		"never had a class comment, no need to write empty string out"

	ptr _ oldCommentRemoteStr ifNil: [0] ifNotNil: [oldCommentRemoteStr sourcePointer].
	SourceFiles ifNotNil: [ | file |
		(file _ SourceFiles at: 2) ifNotNil: [
			file setToEnd; newLine; nextPut: $!!.	"directly"
			"Should be saying (file command: 'H3') for HTML, but ignoring it here"
			header _ String streamContents: [:strm | strm nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
				aStamp storeOn: strm.
				strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
			file nextChunkPut: header]].
	self organization classComment: (RemoteString newString: aString onFileNumber: 2) stamp: aStamp.
	SystemChangeNotifier uniqueInstance classCommented: self.
! !


!Collection methodsFor: 'enumerating'!
inject: thisValue into: binaryBlock 
	"Accumulate a running value associated with evaluating the argument, 
	binaryBlock, with the current value of the argument, thisValue, and the 
	receiver as block arguments. For instance, to sum the numeric elements 
	of a collection, aCollection inject: 0 into: [:subTotal :next | subTotal + 
	next]."

	| nextValue |
	nextValue _ thisValue.
	self do: [:each | nextValue _ binaryBlock value: nextValue value: each].
	^nextValue! !


!CompiledMethod methodsFor: 'comparing' stamp: 'jmv 5/27/2015 14:03'!
= method
	| numLits lit1 lit2 |

	"Any object is equal to itself"
	self == method ifTrue: [ ^ true ].

	"Answer whether the receiver implements the same code as the 
	argument, method."
	(method is: #CompiledMethod) ifFalse: [ ^false ].
	self size = method size ifFalse: [ ^false ].
	self header = method header ifFalse: [ ^false ].
	self initialPC to: self endPC do: [ :i |
		(self at: i) = (method at: i) ifFalse: [ ^false ]].
	(numLits _ self numLiterals) ~= method numLiterals ifTrue: [ ^false ].

	"``Dont bother checking FFI and named primitives''
	jmv: Does this make any sense?
	 (#(117 120) includes: self primitive) ifTrue: [^ true]."

	 "properties"
	(self properties analogousCodeTo: method properties) ifFalse: [
		^false ].

	"#penultimateLiteral is selector (or properties, just compared, above)
	Last literal is #methodClass.
	Don't compare them. Two methods might be equal even if they have different selector (or none at all)
	or are installed in different classes (or none at all)"
	1 to: numLits-2 do: [ :i |
		lit1 _ self literalAt: i.
		lit2 _ method literalAt: i.
		lit1 = lit2 ifFalse: [
			(i = 1 and: [ #(117 120) includes: self primitive ])
				ifTrue: [
					lit1 isArray
						ifTrue: [
							(lit2 isArray and: [ lit1 allButLast = lit2 allButLast ]) ifFalse: [
								^false ]]
						ifFalse: [ "ExternalLibraryFunction"
							(lit1 analogousCodeTo: lit2) ifFalse: [
								^false ]]]
				ifFalse: [
					 lit1 isFloat
						ifTrue: [
							"Floats match if values are close, due to roundoff error."
							(lit1 closeTo: lit2) ifFalse: [ ^false ].
							self flag: 'just checking'. self halt ]
						ifFalse: [
							"any other discrepancy is a failure"
							^ false ]]]].
	^true! !

!CompiledMethod methodsFor: 'literals' stamp: 'jmv 5/27/2015 14:03'!
hasLiteralSuchThat: litBlock
	"Answer true if litBlock returns true for any literal in this method, even if embedded in array structure."
	| lit |
	(self penultimateLiteral isMethodProperties
	 and: [self penultimateLiteral hasLiteralSuchThat: litBlock]) ifTrue:
		[^true].
	2 to: self numLiterals + 1 do: [ :index |
		lit := self objectAt: index.
		((litBlock value: lit)
		or: [lit isArray and: [lit hasLiteralSuchThat: litBlock]]) ifTrue:
			[^true]].
	^false! !

!CompiledMethod methodsFor: 'literals' stamp: 'jmv 5/27/2015 14:03'!
hasLiteralThorough: literal
	"Answer true if any literal in this method is literal,
	even if embedded in array structure."

	| lit |
	(self penultimateLiteral isMethodProperties
	 and: [self penultimateLiteral hasLiteralThorough: literal]) ifTrue:[^true].
	2 to: self numLiterals - 1 "exclude superclass + selector/properties"
	   do: [ :index |
		(((lit := self objectAt: index) literalEqual: literal)
		 or: [(lit isVariableBinding and: [lit key == literal])
		 or: [lit isArray and: [lit hasLiteral: literal]]]) ifTrue:
			[^ true]].
	^ false ! !


!ShortIntegerArray methodsFor: 'objects from disk' stamp: 'jmv 5/27/2015 14:06'!
writeOn: aStream 

	| w |
	aStream nextInt32Put: self basicSize.

	1 to: self basicSize do: [ :i |
		w _ self basicAt: i.
		Smalltalk isLittleEndian
			ifFalse: [ aStream nextNumber: 4 put:  w ]
			ifTrue: [ aStream
				nextPut: (w digitAt: 2);
				nextPut: (w digitAt: 1);
				nextPut: (w digitAt: 4);
				nextPut: (w digitAt: 3) ]].! !


!Set methodsFor: 'enumerating' stamp: 'jmv 5/27/2015 13:49'!
do: aBlock 
	| each |
	tally = 0 ifTrue: [^ self].
	1 to: array size do: [ :index |
		(each _ array at: index) ifNotNil: [aBlock value: each]]! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/27/2015 13:49'!
saveAs: newName andQuit: aBoolean clearAllClassState: clearAllStateFlag
	"Save the image  under a new name."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileDirectory default 
			copyFileWithoutOverwriteConfirmationNamed: oldChangesName
			toFileNamed: newChangesName.
		self setMacFileInfoOn: newChangesName ].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: aBoolean
		clearAllClassState: clearAllStateFlag! !


!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'private' stamp: 'jmv 5/27/2015 13:33'!
privateTempRefsForContext: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents
	"Answer the sequence of temps in scope in aContext in the natural order,
	 outermost arguments and temporaries first, innermost last.  Each temp is
	 a pair of the temp's name followed by a reference.  The reference can be
		integer - index of temp in aContext
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index in aContext
		#( outer. temp reference ) - a temp reference in an outer context."
	blockExtentsToTempRefs ifNil:
		[blockExtentsToTempRefs := (aContext method holdsTempNames
										ifTrue: [aContext method]
										ifFalse: [methodNode]) blockExtentsToTempsMap.
		 blockExtentsToTempRefs ifNil:
			["an empty method.  shouldn't be able to step into here but it
			  can happen in weird circumstances (i.e. with MethodWrapper)."
			blockExtentsToTempRefs := Dictionary new.
			blockExtentsToTempRefs
				at: (theContextsStartpcsToBlockExtents at: aContext startpc)
				put: {}].
		 startpcsToTempRefs := Dictionary new].
	^startpcsToTempRefs
		at: aContext startpc
		ifAbsentPut:
			[| localRefs outerTemps |
			 localRefs := blockExtentsToTempRefs at: (theContextsStartpcsToBlockExtents at: aContext startpc).
			 aContext outerContext
				ifNil: [localRefs]
				ifNotNil:
					[:outer|
					"Present temps in the order outermost to innermost left-to-right, but replace
					 copied outermost temps with their innermost copies"
					 outerTemps := (self
										privateTempRefsForContext: outer
										startpcsToBlockExtents: theContextsStartpcsToBlockExtents) collect:
						[:outerPair|
						localRefs
							detect: [:localPair| outerPair first = localPair first]
							ifNone: [{ outerPair first. { #outer. outerPair last } }]].
					outerTemps,
					 (localRefs reject: [:localPair| outerTemps anySatisfy: [:outerPair| localPair first = outerPair first]])]]! !


!Form methodsFor: 'other' stamp: 'jmv 5/27/2015 14:04'!
preMultiplyAlpha
	"Pre-multiply each pixel by its alpha, for proper alpha compositing (BitBlt rule 34).
	E.g., half-transparent green 16r7F00FF00 becomes 16r7F007F00"

	| v a r g b |
	depth = 32 ifFalse: [^self].
	1 to: bits size do: [ :i |
		v := bits at: i.
		a := v bitShift: -24.
		r := ((v bitShift: -16) bitAnd: 255) * a // 255.
		g := ((v bitShift: -8) bitAnd: 255) * a // 255.
		b := (v bitAnd: 255) * a // 255.
		bits at: i put: (a bitShift: 24) + (r bitShift: 16) + (g bitShift: 8) + b].! !


!RelativeInstructionPrinter methodsFor: 'printing' stamp: 'jmv 5/27/2015 14:06'!
print: instruction 
	"Append to the receiver a description of the bytecode, instruction." 

	| code |
	stream tab: self indent.
	labelling
		ifTrue: [stream print: oldPC - method initialPC; space]
		ifFalse: [stream tab].
	stream tab: (innerIndents at: oldPC).
	self printCode ifTrue: [
		stream nextPut: $<.
		 oldPC to: scanner pc - 1 do: [ :i |
			code := (method at: i) printStringBase: 16.
			stream
				nextPut: (code size < 2 ifTrue: [$0] ifFalse: [code at: 1]);
				nextPut: code last;
				space].
		 stream skip: -1; nextPut: $>; space].
	stream nextPutAll: instruction.
	stream newLine.
	labelling ifFalse: [
		(labels at: scanner pc + 1) ~~ false ifTrue:
			[stream nextPutAll: (labels at: scanner pc + 1); nextPut: $:; newLine]].
	oldPC := scanner pc! !


!Decompiler methodsFor: 'private' stamp: 'jmv 5/27/2015 12:51'!
convertToDoLoop
	"If statements contains the pattern
		var := startExpr.
		[var <= limit] whileTrue: [...statements... var := var + incConst]
	then replace this by
		startExpr to: limit by: incConst do: [:var | ...statements...]"
	| initStmt toDoStmt limitStmt |
	(stack notEmpty
	 and: [(stack last == #CaseFlag) not and: [ stack last isAssignmentNode]])
		ifTrue:
			[initStmt := stack last.
			 (toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			 stack removeLast; addLast: toDoStmt.
			 statements removeLast]
		ifFalse:
			[statements size < 2 ifTrue:
				[^self].
			initStmt := statements at: statements size-1.
			(toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			statements removeLast; removeLast; addLast: toDoStmt].
	initStmt variable scope: -1.  "Flag arg as block temp"

	"Attempt further conversion of the pattern
		limitVar := limitExpr.
		startExpr to: limitVar by: incConst do: [:var | ...statements...]
	to
		startExpr to: limitExpr by: incConst do: [:var | ...statements...].
	The complication here is that limitVar := limitExpr's value may be used, in which case it'll
	be statements last, or may not be used, in which case it'll be statements nextToLast."
	statements size < 2 ifTrue: [^ self].
	limitStmt := statements last.
	((limitStmt isMemberOf: AssignmentNode)
		and: [limitStmt variable isTemp
		and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
			[limitStmt := statements at: statements size-1.
			((limitStmt isMemberOf: AssignmentNode)
				and: [limitStmt variable isTemp
				and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
					[^self]].

	(self blockScopeRefersOnlyOnceToTemp: limitStmt variable fieldOffset) ifFalse:
		[^self].
	toDoStmt arguments at: 1 put: limitStmt value.
	limitStmt variable scope: -2.  "Flag limit var so it won't print"
	statements last == limitStmt
		ifTrue: [statements removeLast]
		ifFalse: [statements removeLast; removeLast; addLast: toDoStmt]! !


!ReferencesExplorerWrapper methodsFor: 'printing' stamp: 'jmv 5/27/2015 13:51'!
referenceDescriptionFrom: parentObject to: childObject count: anInteger

	| parentClass referencesSoFar eachVariable eachSlot eachLiteral |
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
						eachVariable _ ReferenceFinderMirror object: parentObject instVarAt: eachIndex.
						eachVariable == childObject ifTrue:
							[
								referencesSoFar _ referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^parentClass allInstVarNames at: eachIndex]
							]
					].
				1 to: (ReferenceFinderMirror objectSize: parentObject) do:
					[:eachIndex |
						eachSlot _ ReferenceFinderMirror object: parentObject basicAt: eachIndex.
						eachSlot == childObject ifTrue:
							[
								referencesSoFar _ referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^'at: ', eachIndex printString]
							]
					]
			].
	^'unknown (maybe the reference changed)'! !


!MessageTally methodsFor: 'printing' stamp: 'jmv 5/27/2015 13:46'!
printOn: aStream total: total totalTime: totalTime tallyExact: isExact 

	| myTally percentage aSelector aClass |
	isExact 
		ifTrue: [
			myTally := tally.
			receivers
				ifNotNil: [receivers do: [:r | myTally := myTally - r tally]].
			aStream
				print: myTally;
				space]
		ifFalse: [
			percentage := tally asFloat / total * 100.0.
			percentage printOn: aStream fractionDigits: 1.
			aStream
				nextPutAll: '% (';
				nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
				nextPutAll: 'ms) '].
	receivers
		ifNil: [
			aStream
				nextPutAll: 'primitives';
				newLine]
		ifNotNil: [
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: aSelector;
				newLine ]! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 5/27/2015 13:31'!
browseAllMessages
	"Create and schedule a message set browser on all implementors of all the messages sent by the current method."

	| aClass method filteredList |
	model selectedMessageName ifNotNil: [ :aName |
		method _ (aClass _ model selectedClassOrMetaClass) compiledMethodAt: aName.
		filteredList _ method messages reject: 
			[:each | #(new initialize = ) includes: each].
		Smalltalk browseAllImplementorsOfList: filteredList asArray sort
			 title: 'All messages sent in ', aClass name, '.', aName]! !


!ProcessBrowserWindow methodsFor: 'menu building' stamp: 'jmv 5/27/2015 13:48'!
processListMenu
	| menu rules |
	menu _ MenuMorph new defaultTarget: self.

	model selectedProcess
		ifNotNil: [ :selectedProcess |
			rules _ model class rulesFor: model selectedProcess.
			menu addList: #(
				('inspect (i)'					#inspectProcess)
				('explore (I)'				#exploreProcess)
				('references finder'			#openReferencesFinder)).
			rules first
				ifTrue: [
					menu add: 'terminate (t)' target: model action: #terminateProcess.
					selectedProcess isSuspended
						ifTrue: [menu add: 'resume (r)' target: model action: #resumeProcess]
						ifFalse: [menu add: 'suspend (s)' target: model action: #suspendProcess]].
			rules second
				ifTrue: [
					menu addList: #(
						('change priority (p)'		 	#changePriority)
						('debug (d)'						#debugProcess))].
			menu addList: #(('profile messages (m)'	#messageTally)).
			(selectedProcess suspendingList isKindOf: Semaphore)
				ifTrue: [menu add: 'signal Semaphore (S)' target: model action: #signalSemaphore].
			menu add: 'full stack (k)' target: model action: #moreStack.
			menu addLine].

	menu addList: #(
		('find context... (f)'		#findContext)
		('find again (g)'			#nextContext		''		model)).
	menu addLine.

	menu
		add: (isStepping
				ifTrue: ['turn off auto-update (a)']
				ifFalse: ['turn on auto-update (a)'])
		action: #toggleAutoUpdate.
	menu add: 'update list (u)' target: model action: #updateProcessList.

	menu addLine.
	CPUWatcher isMonitoring
			ifTrue: [ menu add: 'stop CPUWatcher' action: #stopCPUWatcher ]
			ifFalse: [ menu add: 'start CPUWatcher' action: #startCPUWatcher  ].

	^ menu! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 5/27/2015 13:42'!
addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint colorToUse form icon |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	handle morphBoundsInWorld: (Rectangle center: aPoint extent: self class handleSize asPoint).
	handleSpec iconSymbol ifNotNil: [ :iconName |
			form _ self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: 0@0 ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^handle! !


!LayoutMorph methodsFor: 'layout' stamp: 'jmv 5/27/2015 13:45'!
layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixed normalizationFactor availableForPropWidth 
		widths theLeft usableHeight boundsTop boundsRight theTop minWidth submorphsToLayout 
			nextMorph ht wd ls|
	
	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableWidth := boundsForLayout width - ((submorphsToLayout size + 1) * xSep).
	sumOfFixed := submorphsToLayout sum: [ :m | m layoutSpec fixedWidth ].
	availableForPropWidth := (usableWidth - sumOfFixed) max: 0.
	normalizationFactor := self proportionalWidthNormalizationFactor.
	availableForPropWidth := availableForPropWidth * normalizationFactor.
	
	widths := submorphsToLayout collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	minWidth := widths sum.
	theLeft := ((usableWidth - minWidth) * (padding ifNil: [0]) + xSep max: 0) 
			+  boundsForLayout left.
	usableHeight := boundsForLayout height - (2*ySep) max: 0.
	boundsTop    := boundsForLayout top.	
	boundsRight  := boundsForLayout right.
	
	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		wd := widths at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		ht := (ls heightFor: usableHeight) min: usableHeight.
		theTop := (usableHeight - ht) * ls minorDirectionPadding + ySep + boundsTop.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		nextMorph morphPosition: theLeft rounded @ theTop rounded extent: (wd rounded min: minWidth)@ ht rounded.
			
		wd > 0 ifTrue: [ theLeft := theLeft + wd + xSep min: boundsRight ]
	]! !

!LayoutMorph methodsFor: 'layout' stamp: 'jmv 5/27/2015 13:44'!
layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixed normalizationFactor availableForPropHeight 
		heights theTop usableWidth boundsLeft boundsBottom theLeft minHeight submorphsToLayout 
			nextMorph ht wd ls|

	boundsForLayout extent > (2@2) "self minimumExtent" 
		ifFalse: [ ^self ]. 	"Too small. Don't bother!!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableHeight := boundsForLayout height - ((submorphsToLayout size + 1) * ySep).
	sumOfFixed := submorphsToLayout sum: [ :m | m layoutSpec fixedHeight ].
	availableForPropHeight := usableHeight - sumOfFixed.
	normalizationFactor := self proportionalHeightNormalizationFactor.
	availableForPropHeight := availableForPropHeight * normalizationFactor.
	
	heights := submorphsToLayout collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	minHeight := heights sum.
	theTop := ((usableHeight - minHeight) * (padding ifNil: [0]) + ySep max: 0) 
				+  boundsForLayout top.
	usableWidth     := boundsForLayout width - (2*xSep) max: 0.
	boundsLeft      := boundsForLayout left.	
	boundsBottom := boundsForLayout bottom.
	
	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		ht := heights at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		wd := (ls widthFor: usableWidth) min: usableWidth.
		theLeft := (usableWidth - wd) * ls minorDirectionPadding + xSep + boundsLeft.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		nextMorph morphPosition: theLeft rounded @ theTop rounded extent: wd rounded @ (ht rounded min: minHeight).
			
		ht > 0 ifTrue: [ theTop := theTop + ht + ySep min: boundsBottom ]
	]! !


!BlockNode methodsFor: 'code generation (new scheme)' stamp: 'jmv 5/27/2015 14:01'!
emitCodeExceptLast: stack encoder: encoder
	| position nextToLast statement |
	position := stack position.
	nextToLast := statements size - 1.
	1 to: nextToLast do: [ :i |
		statement := statements at: i.
		statement emitCodeForEffect: stack encoder: encoder.
		self assert: stack position = position].! !

!BlockNode methodsFor: 'code generation (new scheme)' stamp: 'jmv 5/27/2015 14:02'!
sizeCodeExceptLast: encoder
	| codeSize statement |
	codeSize := 0.
	1 to: statements size - 1 do: [ :i |
		 statement := statements at: i.
		 codeSize := codeSize + (statement sizeCodeForEffect: encoder)].
	^codeSize! !


!ReferenceFinder methodsFor: 'private - references' stamp: 'jmv 5/27/2015 13:48'!
breadthFirstTrace
	"Using to:do: is ~10% faster than timesRepeat:"

	| next segment |
	self scanQueueAdd: self rootNode.
	[self scanQueue first isEmpty] whileFalse: [
			segment _ self scanQueueRemoveFirstSegment.
			1 to: segment size do: [ :some |
					next _ segment at: some.
					next breadthFirstFindReferencesTo: self target for: self.
				].
			self scanQueueRecycledSegment: segment
		]! !


!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 5/27/2015 14:06'!
isBinary
	| c |
	(currentToken isNil or: [self isName or: [self isKeyword]]) 
		ifTrue: [^false].
	1 to: currentToken size do: [ :i |
		c := currentToken at: i.
		((self isSelectorCharacter: c) or: [i = 1 and: [c == $-]]) 
			ifFalse: [^false]].
	^true! !


!Parser methodsFor: 'error correction' stamp: 'jmv 5/27/2015 13:47'!
correctVariable: proposedVariable interval: spot
	"Correct the proposedVariable to a known variable, or declare it as a new
	variable if such action is requested.  We support declaring lowercase
	variables as temps or inst-vars, and uppercase variables as Globals or 
	ClassVars, depending on whether the context is nil (class=UndefinedObject).
	Spot is the interval within the test stream of the variable.
	rr 3/4/2004 10:26 : adds the option to define a new class. "

	"Check if this is an i-var, that has been corrected already (ugly)"

	"Display the pop-up menu"

	| userSelection action |
	(encoder classEncoding instVarNames includes: proposedVariable) ifTrue: [
		^InstanceVariableNode new 
			name: proposedVariable
			index: (encoder classEncoding allInstVarNames indexOf: proposedVariable)].

	"First check to see if the requestor knows anything about the variable"
	(requestor bindingOf: proposedVariable)
		ifNotNil: [ :binding | ^ encoder global: binding name: proposedVariable].

	"If we can't ask the user for correction, make it undeclared"
	self interactive ifFalse: [^encoder undeclared: proposedVariable].

	userSelection _ requestor selectionInterval.
	requestor selectFrom: spot first to: spot last.

	"Build the menu with alternatives"
	action _ UndeclaredVariable 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.
	^action value! !


!TextComposition methodsFor: 'selection' stamp: 'jmv 5/27/2015 13:49'!
addSelectionRectsFrom: characterBlock1 to: characterBlock2 to: aStream
	"Return an array of rectangles representing the area between the two character blocks given as arguments."
	| line1 line2 rects cb1 cb2 w line |
	characterBlock1 <= characterBlock2
		ifTrue: [cb1 _ characterBlock1.  cb2 _ characterBlock2]
		ifFalse: [cb2 _ characterBlock1.  cb1 _ characterBlock2].
	cb1 = cb2 ifTrue: [
		w _ 6.
		^ aStream nextPut: (cb1 topLeft - (w@0) corner: cb1 bottomLeft + ((w+1)@0))].
	line1 _ self lineIndexFor: cb1 stringIndex.
	line2 _ self lineIndexFor: cb2 stringIndex.
	line1 = line2 ifTrue: [
		^ aStream nextPut: (cb1 topLeft corner: cb2 bottomRight)].
	rects _ OrderedCollection new.
	rects addLast: (cb1 topLeft corner: (lines at: line1) bottomRight).
	line1+1 to: line2-1 do: [ :i |
		line _ lines at: i.
		(line left = rects last left and: [ line right = rects last right ])
			ifTrue: [ "new line has same margins as old one -- merge them, so that the caller gets as few rectangles as possible"
					| lastRect |
					lastRect _ rects removeLast.
					rects add: (lastRect bottom: line bottom) ]
			ifFalse: [ "differing margins; cannot merge"
					rects add: line rectangle ] ].
	aStream nextPutAll: rects.
	aStream nextPut: ((lines at: line2) topLeft corner: cb2 bottomLeft)! !


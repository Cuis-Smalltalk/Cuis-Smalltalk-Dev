'From Cuis 5.0 [latest update: #5013] on 30 December 2021 at 9:41:03 am'!

!ClassDescription methodsFor: 'initialization' stamp: 'jmv 12/29/2021 17:47:34'!
canUpdateFrom: oldClass
	"If some compiled method that will become invalid is currently being executed,
	the shape conversion of instances is extremely insecure.
	Answer if migration of instances looks safe"
	| answer newMethod oldMethod selector newActualClass oldActualClass |
	answer _ true.
	Processor 
		processesDo: [ :p | ]
		withStackFramestDo: [ :process :context |
			(context receiver isKindOf: oldClass) ifTrue: [
				oldActualClass _ context receiver class.
				self allSubclassesDo: [ :c |
					c name = oldActualClass name ifTrue: [
						newActualClass _ c ]].
				selector _ context method selector.
				oldMethod _ oldActualClass lookupSelector: selector.
				newMethod _ newActualClass lookupSelector: selector.
				oldMethod = newMethod ifFalse: [
					('Class reshaping: ', context receiver class name, ' has some instance running #', selector, '. This CompiledMethod would become invalid. Class reshaping aborted.') print.
					answer _ false ].
				]]
		runningProcessSearchStart: nil.
	^answer! !


!ProtoObject methodsFor: 'system primitives' stamp: 'jmv 12/29/2021 17:48:30'!
become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	| selfMethod otherObjectMethod selector contextReceiver |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				contextReceiver _ context receiver.
				(self == contextReceiver or: [ otherObject == contextReceiver ]) ifTrue: [
					selector _ context method selector.
					selfMethod _ self class lookupSelector: selector.
					otherObjectMethod _ otherObject class lookupSelector: selector.
					selfMethod = otherObjectMethod ifFalse: [
						MethodInCallStackToBecomeInvalid
							signal: contextReceiver class name, ' has some instance running #', selector, '. This CompiledMethod would become invalid. #become: aborted.' ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsExchangeIdentityWith: { otherObject }! !


!Object methodsFor: 'system primitives' stamp: 'jmv 12/29/2021 17:48:42'!
becomeForward: otherObject 
	"Primitive. All variables in the entire system that used to point
	to the receiver now point to the argument.
	Fails if either argument is a SmallInteger."

	| newMethod oldMethod selector |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				self == context receiver ifTrue: [
					selector _ context method selector.
					oldMethod _ self class lookupSelector: selector.
					newMethod _ otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						MethodInCallStackToBecomeInvalid
							signal: self class name, ' has some instance running #', selector, '. This CompiledMethod would become invalid. #becomeForward: aborted.' ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsForwardIdentityTo: { otherObject }! !

!Object methodsFor: 'system primitives' stamp: 'jmv 12/29/2021 17:48:56'!
becomeForward: otherObject copyHash: copyHash
	"Primitive. All variables in the entire system that used to point to the receiver now point to the argument.
	If copyHash is true, the argument's identity hash bits will be set to those of the receiver.
	Fails if either argument is a SmallInteger."

	| newMethod oldMethod selector |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :proces :context |
				self == context receiver ifTrue: [
					selector _ context method selector.
					oldMethod _ self class lookupSelector: selector.
					newMethod _ otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						MethodInCallStackToBecomeInvalid
							signal: self class name, ' has some instance running #', selector, '. This CompiledMethod would become invalid. #becomeForward:copyHash: aborted.' ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self }
		elementsForwardIdentityTo: { otherObject }
		copyHash: copyHash! !


!ClassDescription methodsFor: 'initialization' stamp: 'jmv 12/29/2021 17:49:16'!
updateInstancesFrom: oldClass
	"Recreate any existing instances of the argument, oldClass, as instances of 
	 the receiver, which is a newly changed class. Permute variables as necessary,
	 and forward old instances to new instances.. Answer nil to defeat any clients
	 that expected the old behaviour of answering the array of old instances."
	"ar 7/15/1999: The updating below is possibly dangerous. If there are any
	contexts having an old instance as receiver it might crash the system if
	the new receiver in which the context is executed has a different layout.
	See bottom below for a simple example:
	jmv 1/29/2019: Detect the problem and prevent mutation if that is the case."
	| allInstances newMethod oldMethod selector |
	allInstances _ oldClass allInstances.
	allInstances notEmpty ifTrue: [
		Processor 
			processesDo: [ :p | ]
			withStackFramestDo: [ :process :context |
				(context receiver isKindOf: oldClass) ifTrue: [
					selector _ context method selector.
					oldMethod _ oldClass lookupSelector: selector.
					newMethod _ self lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						MethodInCallStackToBecomeInvalid
							signal: context receiver class name, ' has some instance running #', selector, '. This CompiledMethod would become invalid. Class reshaping aborted.' ]]]
			runningProcessSearchStart: nil.
		self updateInstances: allInstances asArray from: oldClass isMeta: self isMeta ].
	^nil

"This attempts to crash the VM by stepping off the end of an instance.
 As the doctor says, do not do this."
"	| crashingBlock class |
	class := Object subclass: #CrashTestDummy
		instanceVariableNames: 'instVar'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	class compile:'instVar: value instVar := value'.
	class compile:'crashingBlock ^[instVar]'.
	crashingBlock := (class new) instVar: 42; crashingBlock.
	Object subclass: #CrashTestDummy
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	crashingBlock value"! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 12/29/2021 18:18:49'!
class: oldClass instanceVariableNames: instVarString unsafe: unsafe
	"This is the basic initialization message to change the definition of
	an existing Metaclass"
	| instVars newClass needNew copyOfOldClass |
	instVars _ Scanner new scanFieldNames: instVarString.
	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateInstvars: instVars from: oldClass forSuper: oldClass superclass) ifFalse:[^nil].
		(self validateSubclassFormat: oldClass typeOfClass from: oldClass forSuper: oldClass superclass extra: instVars size) ifFalse:[^nil]].
	"See if we need a new subclass or not"
	needNew _ self needsSubclassOf: oldClass superclass type: oldClass typeOfClass instanceVariables: instVars from: oldClass.
	needNew ifNil:[^nil]. "some error"
	needNew ifFalse:[^oldClass]. "no new class needed"

	"Create the new class"
	copyOfOldClass _ oldClass copy.
	newClass _ self 
		newSubclassOf: oldClass superclass 
		type: oldClass typeOfClass
		instanceVariables: instVars
		from: oldClass.
		
	newClass _ self recompile: false from: oldClass to: newClass mutate: false.
	newClass isNil ifTrue: [ ^nil ].

	self doneCompiling: newClass.
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	^newClass! !

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 12/29/2021 18:19:14'!
name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory |
	
	instVars _ Scanner new scanFieldNames: instVarString.
	classVars _ (Scanner new scanFieldNames: classVarString) collect: [:x | x asSymbol].

	"Validate the proposed name"
	unsafe ifFalse:[(self validateClassName: className) ifFalse:[^nil]].
	oldClass _ Smalltalk at: className ifAbsent: nil.
	oldClass isBehavior 
		ifFalse:[oldClass _ nil]. "Already checked in #validateClassName:"
	copyOfOldClass _ oldClass copy.

	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateSuperclass: newSuper forSubclass: oldClass) ifFalse:[^nil].
		(self validateInstvars: instVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateClassvars: classVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil]].

	"See if we need a new subclass"
	needNew _ self needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass.
	needNew ifNil: [^nil]. "some error"

	(needNew and:[unsafe not]) ifTrue:[
		"Make sure we don't redefine any dangerous classes"
		(self tooDangerousClasses includes: oldClass name) ifTrue:[
			self error: oldClass name, ' cannot be changed'.
		].
		"Check if the receiver should not be redefined"
		(oldClass notNil and:[oldClass shouldNotBeRedefined]) ifTrue:[
			self notify: oldClass name asText allBold, 
						' should not be redefined!! \Proceed to store over it.' withNewLines]].

	needNew ifTrue:[
		"Create the new class"
		newClass _ self 
			newSubclassOf: newSuper 
			type: type 
			instanceVariables: instVars
			from: oldClass.
		newClass ifNil: [ ^nil]. "Some error"
		newClass setName: className.
	] ifFalse:[
		"Reuse the old class"
		newClass _ oldClass.
	].

	"Install the class variables and pool dictionaries... "
	force _ (newClass declare: classVarString) | (newClass sharing: poolString).

	"... classify ..."
	newCategory _ category withBlanksTrimmed.
	organization _ Smalltalk organization.
	oldClass ifNotNil: [oldCategory := (organization categoryOfElement: oldClass name) asSymbol].
	organization classify: newClass name under: newCategory.

	"... recompile ..."
	newClass _ self recompile: force from: oldClass to: newClass mutate: false.
	newClass isNil ifTrue: [ ^nil ].

	"... export if not yet done ..."
	(Smalltalk at: newClass name ifAbsent: nil) == newClass ifFalse:[
		[Smalltalk at: newClass name put: newClass]
			on: AttemptToWriteReadOnlyGlobal do:[:ex| ex resume: true].
		Smalltalk flushClassNameCache.
	].

	self doneCompiling: newClass.
	
	"... notify interested clients ..."
	oldClass ifNil: [
		SystemChangeNotifier uniqueInstance classAdded: newClass inCategory: newCategory.
		^ newClass].
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	newCategory ~= oldCategory 
		ifTrue: [SystemChangeNotifier uniqueInstance classRecategorized: newClass from: oldCategory to: newCategory ].
	^newClass! !

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 12/29/2021 18:17:37'!
recompile: force from: oldClass to: newClass mutate: forceMutation
	"Do the necessary recompilation after changing oldClass to newClass.
	If required (e.g., when oldClass ~~ newClass) mutate oldClass to newClass
	and all its subclasses. If forceMutation is true force a mutation even
	if oldClass and newClass are the same."

	| compilationSuccess compilationErrorDescription |
	oldClass
		ifNil: [^ newClass].

	(newClass == oldClass and:[force not and:[forceMutation not]]) ifTrue:[
		^newClass].

	currentClassIndex _ 0.
	maxClassIndex _ oldClass withAllSubclasses size.

	(oldClass == newClass and:[forceMutation not]) ifTrue:[
		"Recompile from newClass without mutating"
		self informUserDuring:[
			newClass withAllSubclassesDo: [ :cl |
				self showProgressFor: cl.
				cl compileAll]].
		^newClass].
	"Recompile oldClass to newClass"
	self informUserDuring: [
		compilationSuccess _ true.
		[self createHierarchy: oldClass to: newClass] on: Error do: [ :exception |
			"If compilation failed, don't continue!!"
			compilationErrorDescription _ exception printString.
			compilationSuccess _ false ].
	].

	compilationSuccess ifFalse: [
		oldClass superclass removeSubclass: newClass.
		Smalltalk garbageCollect.
		Error signal: compilationErrorDescription, '. Please see Transcript.'.
		^ nil ].

	(newClass canUpdateFrom: oldClass) ifFalse: [
		oldClass superclass removeSubclass: newClass.
		Smalltalk garbageCollect.
		MethodInCallStackToBecomeInvalid
			signal: oldClass name, ' has some subInstance running a CompiledMethod that would become invalid. Please see Transcript.'.
		^nil ].

	"Mutate oldClass to newClass"
	self informUserDuring: [
		self mutateInstances: oldClass to: newClass.
	].
	^oldClass "now mutated to newClass"! !

!ClassBuilder methodsFor: 'class mutation' stamp: 'jmv 12/30/2021 09:39:17'!
update: oldClass to: newClass
	"Convert oldClass, all its instances and possibly its meta class into newClass,
	 instances of newClass and possibly its meta class. The process is surprisingly
	 simple in its implementation and surprisingly complex in its nuances and potentially
	 bad side effects.
	 We can rely on two assumptions (which are critical):
		#1: The method #updateInstancesFrom: will not create any lasting pointers to
			 'old' instances ('old' is quote on quote since #updateInstancesFrom: will do
			 a become of the old vs. the new instances and therefore it will not create
			 pointers to *new* instances before the #become: which are *old* afterwards)
		#2: The non-preemptive execution of the critical piece of code guarantees that
			 nobody can get a hold by 'other means' (such as process interruption and
			 reflection) on the old instances.
	 Given the above two, we know that after #updateInstancesFrom: there are no pointers
	 to any old instances. After the forwarding become there will be no pointers to the old
	 class or meta class either.
	<preSpur> Meaning that if we throw in a nice fat GC at the end of the critical block, everything will 
	be gone (but see the comment right there). </preSpur> 
	 Andreas Raab, 2/27/2003 23:42"
	| meta allInstances |
	meta _ oldClass isMeta.
	"Note: Everything from here on will run without the ability to get interrupted
	to prevent any other process to create new instances of the old class."
	["Note: The following removal may look somewhat obscure and needs an explanation.
	  When we mutate the class hierarchy we create new classes for any existing subclass.
	  So it may look as if we don't have to remove the old class from its superclass. However,
	  at the top of the hierarchy (the first class we reshape) that superclass itself is not newly
	  created so therefore it will hold both the oldClass and newClass in its (obsolete or not)
	  subclasses. Since the #become: below will transparently replace the pointers to oldClass
	  with newClass the superclass would have newClass in its subclasses TWICE. With rather
	  unclear effects if we consider that we may convert the meta-class hierarchy itself (which
	  is derived from the non-meta class hierarchy).
	  Due to this problem ALL classes are removed from their superclass just prior to converting
	  them. Here, breaking the superclass/subclass invariant really doesn't matter since we will
	  effectively remove the oldClass 
	(<spur>becomeForward:</spur> or <preSpur>become+GC</preSpur>) just a few lines below."

		"Convert the instances of oldClass into instances of newClass.
		Note: #canUpdateFrom: has already been called. Instance mutation is safe wrt methods currently in execution."
		allInstances _ oldClass allInstances.
		allInstances notEmpty ifTrue: [
			newClass updateInstances: allInstances asArray from: oldClass isMeta: meta ].

		oldClass superclass removeSubclass: oldClass.
		oldClass superclass removeObsoleteSubclass: oldClass.

		"make sure that the VM cache is clean"
		oldClass methodDict do: [:cm | cm flushCache].

		meta
			ifTrue:
				[oldClass becomeForward: newClass.
				 oldClass updateMethodBindingsTo: oldClass binding]
			ifFalse:
				[{oldClass. oldClass class} elementsForwardIdentityTo: {newClass. newClass class}.
				 oldClass updateMethodBindingsTo: oldClass binding.
				 oldClass class updateMethodBindingsTo: oldClass class binding].

 Smalltalk isSpur
	ifTrue: [
		"eem 5/31/2014 07:22 At this point there used to be a garbage collect whose purpose was
		 to ensure no old instances existed after the becomeForward:.  Without the GC it was possible
		 to resurrect old instances using e.g. allInstancesDo:.  This was because the becomeForward:
		 updated references from the old objects to new objects but didn't destroy the old objects.
		 But as of late 2013/early 2014 becomeForward: has been modified to free all the old objects."
		]

	ifFalse: [
		"jmv: Squeak 4.6 (pre Spur) includes the GC. So, do it if not Spur.
		Not really sure if needed on newer Cog and Stack non-Spur VMs.
		Not sure if needed for SqueakJS.
		Remove it when we are sure.
		
		Original note by Andreas Raab below."
		Smalltalk garbageCollect.
		"Warning: Read this before you even think about removing the GC. Yes, it slows us down. Quite heavily if you have a large image. However, there's no good and simple alternative here, since unfortunately, #become: does change class pointers. What happens is that after the above become all of the instances of the old class will have a class pointer identifying them as instances of newClass. If we get our hands on any of these instances we will break immediately since their expected instance layout (that of its class, e.g., newClass) will not match their actual instance layout (that of oldClass). And getting your hands on any of those instances is really simple - just reshaping one class two times in rapid succession will do it. Reflection techniques, interrupts, etc. will only add to this problem. In the case of Metaclass things get even worse since when we recompile the entire class hierarchy we will recompile both, Metaclass and its instances (and some of its instances will have the old and some the new layout).
		The only easy solution to this problem would be to 'fix up' the class pointers of the old instances to point to the old class (using primitiveChangeClassTo:). But this won't work either - as we do a one-way become we would have to search the entire object memory for the oldClass and couldn't even clearly identify it unless we give it some 'special token' which sounds quite error-prone. If you really need to get rid of the GC here are some alternatives:
		On the image level, one could create a copy of the oldClass before becoming it into the new class and, after becoming it, 'fix up' the old instances. That would certainly work but it sounds quite complex, as we need to make sure we're not breaking any of the superclass/subclass meta/non-meta class variants.
		Alternatively, fix up #becomeForward on the VM-level to 'dump the source objects' of #become. This would be quite doable (just 'convert' them into a well known special class such as bitmap) yet it has problems if (accidentally or not) one of the objects in #become: appears on 'both sides of the fence' (right now, this will work ... in a way ... even though the consequences are unclear).
		Another alternative is to provide a dedicated primitive for this (instead of using it implicitly in become) which would allow us to dump all the existing instances right here. This is equivalent to a more general primitiveChangeClassTo: and might be worthwhile but it would likely have to keep in mind the differences between bits and pointer thingies etc.
		Since all of the alternatives seem rather complex and magical compared to a straight-forward GC it seems best to stick with the GC solution for now. If someone has a real need to fix this problem, that person will likely be motivated enough to check out the alternatives. Personally I'd probably go for #1 (copy the old class and remap the instances to it) since it's a solution that could be easily reverted from within the image if there's any problem with it.
		 Andreas Raab, 2/27/2003 23:42"
		]
	] valueUnpreemptively! !


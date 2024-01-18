'From Cuis6.3 [latest update: #6163] on 17 January 2024 at 8:56:28 pm'!

!SystemDictionary methodsFor: 'class names' stamp: 'HAW 1/17/2024 20:42:01'!
classNamed: className 
	"className is either a class name or a class name followed by ' class'.
	Answer the class or metaclass it names.
	Answer nil if no class by that name."
	"
	Smalltalk classNamed: #Point
	Smalltalk classNamed: 'Point'
	Smalltalk classNamed: 'Point class'
	Smalltalk classNamed: 'BogusClassName'
	Smalltalk classNamed: 'BogusClassName class'

	Smalltalk classNamed: #Display
	Smalltalk classNamed: 'Display'
	Smalltalk classNamed: 'Display class'
	"

	self at: className asSymbol ifPresent: [ :found |
		^ found isBehavior ifTrue: [ found ]].

	(className withoutSuffix: ' class') ifNotNil: [ :baseName |
		self at: baseName asSymbol ifPresent: [ :found |
			^ found isBehavior ifTrue: [ found class ]]].

	^ nil! !

!SystemDictionary methodsFor: 'class names' stamp: 'HAW 1/17/2024 20:50:04'!
renameAndUpdateReferences: oldName as: newName
	"Dangerous. Simple search and replace could break stuff.
	Use only to generate change sets to later checked with care
	
OldMorph withAllSubclasses do: [ :c |
	n := c name.
	(n beginsWith: 'Old') ifTrue: [
		Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]]

OldBorderStyle withAllSubclasses do: [ :c |
	n := c name.
	(n beginsWith: 'Old') ifTrue: [
		Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]]

#(#OldKeyboardBuffer #OldLayoutCell #OldLayoutFrame #OldLayoutPolicy #OldLayoutProperties #OldMorphExtension #OldMouseClickState #OldProportionalLayout #OldTableLayout #OldTableLayoutProperties #OldTextAnchor #OldTextComposer #OldTextContainer #OldTextLine #OldTheWorldMenu #OldWorldState #OldWorldTest) do: [ :n |
	Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]
	"

	self renameClassNamed: oldName as: newName.
	(self allCallsOn: (self associationAt: newName)) do: [ :methodRef |
		methodRef updateReferencesTo: oldName toBe: newName ]! !

!SystemDictionary methodsFor: 'class names' stamp: 'HAW 1/17/2024 20:50:21'!
renamedClass: aClass from: oldName 
	
	| newName |
	
	newName := aClass name.
	
	SystemChangeNotifier uniqueInstance classRenamed: aClass from: oldName to: newName inCategory: aClass category.
	self 
		logChange: 'Smalltalk renameClassNamed: #', oldName, ' as: #', newName
		preamble: 'classRenamed: #', oldName, ' as: #', newName, Utilities changeStampField! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:41:03'!
browseEqEqSentToCharacterConstants
	"
	Smalltalk browseEqEqSentToCharacterConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isConstantCharacter or: [ node arguments first isConstantCharacter ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:41:08'!
browseEqEqSentToSmallIntegerConstants
	"
	Smalltalk browseEqEqSentToSmallIntegerConstants
	"
	| hasMatch visitor |
	hasMatch := false.
	visitor := ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isConstantNumber or: [ node arguments first isConstantNumber ]]]) ifTrue: [
						hasMatch := true ]]
		select: [ :node |
			hasMatch not ].
	self browseAllSelect: [ :method |
		hasMatch := false.
		method decompile accept: visitor.
		hasMatch ].! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:41:44'!
browseUndeclaredReferences
	"
	Smalltalk browseUndeclaredReferences
	"
	| anythingToShow |
	self cleanOutUndeclared.
	anythingToShow := false.
	Undeclared keys do: [ :k |
		anythingToShow := true.
		self
			browseMessageList: (self allCallsOn: (Undeclared associationAt: k))
			name: 'References to Undeclared: ', k printString ].

	"undeclared not in Undeclared - "
	"This happened in Pharo. See http://lists.gforge.inria.fr/pipermail/pharo-project/2012-March/061270.html
	Maybe do something like this in Cuis too???
	lostUndeclared := self allSelect: [:m|
		m literals anySatisfy: [:l|
			l isVariableBinding
				and: [l key isSymbol ""avoid class-side methodClass literals""
				and: [(m methodClass bindingOf: l key) isNil
				and: [(Undeclared includesAssociation: l) not]]]]].
	"

	anythingToShow ifFalse: [
		 (PopUpMenu labels: ' OK ')
				startUpWithCaption: 'There are no Undeclared at all' ]! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:42:21'!
cleanCompactObsoleteClasses

	| cct |
	"No Compact Classes support in Spur"
	self isSpur ifTrue: [ ^ self ].

	cct := self compactClassesArray.
	cct do: [ :c |
		c ifNotNil: [
			c isObsolete ifTrue: [ 
				cct at: c indexIfCompact put: nil ]]]! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:42:48'!
condenseChanges
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount oldChangesLocalName oldChangesPathName |
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' forceWriteStreamDo: [ :f |
		f timeStamp.
		'Condensing Changes File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: self classNames size
			during: [ :barBlock |
				classCount := 0.
				self allClassesDo: [ :class | 
					barBlock value: (classCount := classCount + 1).
					class moveChangesTo: f.
					class putClassCommentToCondensedChangesFile: f.
					class class moveChangesTo: f ]].
		LastQuitLogPosition := f position ].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	oldChanges close.
	(oldChangesPathName, '.old') asFileEntry delete.
	oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old'.
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: oldChangesPathName asFileEntry appendStream.

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:42:59'!
condenseSources: newVersionString
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount oldChanges oldChangesLocalName oldChangesPathName newChangesPathName newSourcesName |
	SourceFileVersionString := newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName := self defaultSourcesName.
	newSourcesName asFileEntry writeStreamDo: [ :f |
		f timeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: self classNames size
			during: [ :barBlock |
				classCount := 0.
				self allClassesDo: [ :class |
					barBlock value: (classCount := classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChanges := SourceFiles at: 2.
	oldChangesPathName := oldChanges name.
	oldChangesLocalName := oldChanges localName.
	self closeSourceFiles.
	oldChangesPathName ifNotNil: [
		(oldChangesPathName, '.old') asFileEntry delete.
	oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old' ].
	newChangesPathName := self defaultChangesName.
	newChangesPathName asFileEntry writeStreamDo: [ :stream |
		stream timeStamp ].
	LastQuitLogPosition := 0.

	self openSourceFiles.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:45:00'!
macroBenchmark1    "Smalltalk macroBenchmark1"
	"Decompiles and prettyPrints the source for every method in the system (or less depending on the *FILTER*, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same.  Because it never installs the new method, it should not cause any flusing of the method cache."
	 | methodNode oldMethod newMethod badOnes oldCodeString n classes |
	classes := self allClasses select: [:c | c name < 'B3'].
	badOnes := OrderedCollection new.
'Decompiling and recompiling...'
displayProgressAt: Sensor mousePoint
from: 0 to: (classes sum: [:c | c selectors size] ifEmpty: [0])
during: [:barBlock | n := 0.
	classes do:
		[:cls | 
		"Transcript cr; show: cls name."
		cls selectors do:
			[:selector | barBlock value: (n := n+1).
			oldMethod := cls compiledMethodAt: selector.
			oldCodeString := (cls decompilerClass new
								decompile: selector in: cls method: oldMethod)
							decompileString.
			methodNode := cls compilerClass new
						compile: oldCodeString
						in: cls notifying: nil ifFail: nil.
			newMethod := methodNode generate: #(0 0 0 0).
			oldCodeString = (cls decompilerClass new
								decompile: selector in: cls method: newMethod)
							decompileString ifFalse: [badOnes add: cls name , ' ' , selector]]].
].
	^ badOnes size! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:45:06'!
obsoleteBehaviors
	"
	Smalltalk obsoleteBehaviors inspect
	Find all obsolete behaviors including meta classes
	"
	| obs |
	obs := OrderedCollection new.
	self garbageCollect.
	self allObjectsDo: [ :cl |
		(cl isBehavior and: [cl isObsolete]) ifTrue: [obs add: cl]].
	^ obs asArray! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:45:21'!
obsoleteClasses   "Smalltalk obsoleteClasses inspect"
	"NOTE:  Also try inspecting comments below"
	| obs c |
	obs := OrderedCollection new.  
	self garbageCollect.
	Metaclass allInstancesDo:
		[:m | c := m soleInstance.
		(c notNil and: ['AnOb*' match: c name asPlainString])
			ifTrue: [obs add: c]].
	^ obs asArray

"Likely in a ClassDict or Pool...
(Association allInstances select: [:a | (a value isKindOf: Class) and: ['AnOb*' match: a value name]]) asArray
"
"Obsolete class refs or super pointer in last lit of a method...
| n l found |
Smalltalk browseAllSelect:
	[:m | found := false.
	1 to: m numLiterals do:
		[:i | (((l := m literalAt: i) isMemberOf: Association)
				and: [(l value isKindOf: Behavior)
				and: ['AnOb*' match: l value name]])
			ifTrue: [found := true]].
	found]
"! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:49:14'!
removeEmptyMessageCategories
	"Smalltalk removeEmptyMessageCategories"
	self garbageCollect.
	(ClassOrganizer allInstances copyWith: SystemOrganization) do:
		[:org | org removeEmptyCategories]! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:52:04'!
testDecompiler
	"
	Smalltalk testDecompiler
	"
	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."
	| methodNode oldMethod newMethod badOnes oldCodeString n |
	badOnes := OrderedCollection new.
	'Decompiling all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n := 0.
			self allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n := n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					oldMethod := cls compiledMethodAt: selector.
					oldCodeString := (cls decompilerClass new
						decompile: selector
						in: cls
						method: oldMethod) decompileString.
					methodNode := cls compilerClass new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod := methodNode generate: #(0 0 0 0 ).
					oldCodeString =
						(cls decompilerClass new
							decompile: selector
							in: cls
							method: newMethod) decompileString ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: (MethodReference class: cls selector: selector) ]]]].
	self
		browseMessageList: badOnes
		name: 'Decompiler Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:52:15'!
testFormatter
	"Smalltalk testFormatter"
	"Reformats the source for every method in the system, and then compiles that source and verifies that it generates identical code.
	The formatting used will be classic monochrome."
	| newCodeString methodNode oldMethod newMethod badOnes n |
	badOnes := OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n := 0.
			self allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n := n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					newCodeString := cls compilerClass new
						format: (cls sourceCodeAt: selector)
						in: cls
						notifying: nil.
					methodNode := cls compilerClass new
						compile: newCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod := methodNode generate: #(0 0 0 0 ).
					oldMethod := cls compiledMethodAt: selector.
					oldMethod = newMethod ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	self
		browseMessageList: badOnes
		name: 'Formatter Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:52:24'!
testFormatter2
	"Smalltalk testFormatter2"
	"Reformats the source for every method in the system, and then verifies that the order of source tokens is unchanged.
	The formatting used will be classic monochrome"
	| newCodeString badOnes n oldCodeString oldTokens newTokens |
	badOnes := OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n := 0.
			self allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n := n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					oldCodeString := (cls sourceCodeAt: selector) asPlainString.
					newCodeString := cls compilerClass new
						format: oldCodeString
						in: cls
						notifying: nil.
					oldTokens := oldCodeString findTokens: Character separators.
					newTokens := newCodeString findTokens: Character separators.
					oldTokens = newTokens ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	self
		browseMessageList: badOnes
		name: 'Formatter Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 1/17/2024 20:52:35'!
verifyChanges		"Smalltalk verifyChanges"
	"Recompile all methods in the changes file."
	self allBehaviorsDo: [:class | class recompileChanges].
! !

!SystemDictionary methodsFor: 'image, changes name' stamp: 'HAW 1/17/2024 20:32:40'!
userChangesFileName
	"
	Smalltalk userChangesFileName
	"
	^(FileIOAccessor default baseNameFor: self imageName pathAndLocalName second), 
		(Preferences at: #userChangesFileNameExtension)! !

!SystemDictionary methodsFor: 'memory space' stamp: 'HAW 1/17/2024 20:44:51'!
lowSpaceWatcher
	"Wait until the low space semaphore is signalled, then take appropriate actions."

	| p |
	self garbageCollectMost <= self lowSpaceThreshold ifTrue: [
		self garbageCollect <= self lowSpaceThreshold ifTrue: [
			"free space must be above threshold before starting low space watcher"
			^ self primitiveBeep ]].

	LowSpaceSemaphore := Semaphore new.
	self primLowSpaceSemaphore: LowSpaceSemaphore.
	self primSignalAtBytesLeft: self lowSpaceThreshold.  "enable low space interrupts"

	LowSpaceSemaphore wait.  "wait for a low space condition..."

	self primSignalAtBytesLeft: 0.  "disable low space interrupts"
	self primLowSpaceSemaphore: nil.
	LowSpaceProcess := nil.
	"Note: user now unprotected until the low space watcher is re-installed"

	p := (self specialObjectsArray at: 23) ifNil: [
		"In case the LowSpaceSemaphore is signaled not by the VM but by someone else."
		Processor preemptedProcess ].
	self specialObjectsArray at: 23 put: nil.
	p isTerminated ifFalse: [
		p suspend.
		Debugger openProcess: p context: p suspendedContext label: 'Space is low' ]! !

!SystemDictionary methodsFor: 'code authors' stamp: 'HAW 1/17/2024 20:35:40'!
allContributors
"
	Smalltalk allContributors
"
	| answer author |
	answer := Set new.
	self allBehaviorsDo: [ :behavior |
		behavior methodsDo: [ :compiledMethod |
			author := compiledMethod author.
			author notEmpty ifTrue: [
				answer add: author ]]].
	^answer! !

!SystemDictionary methodsFor: 'code authors' stamp: 'HAW 1/17/2024 20:43:06'!
contributionsOf: aString
	"
	Smalltalk contributionsOf: 'JMV'
	"
	| author answer |
	answer := OrderedCollection new.
	self allBehaviorsDo: [ :behavior |
		behavior methodsDo: [ :compiledMethod |
			author := compiledMethod author.
			aString = author ifTrue: [
				answer add: {compiledMethod methodClass. compiledMethod selector}]]].
	^answer! !

!SystemDictionary methodsFor: 'code authors' stamp: 'HAW 1/17/2024 20:52:32'!
unknownContributors
	"Answer a collection of authorInitials for whom there is code in the system 
	(either in core or in loaded packages), but we don't knwo their full name.
	Smalltalk unknownContributors
	"

	| all ok |
	all := self allContributors asSet.
	ok := (self knownInitialsAndNames collect: [ :pair | pair first ]) asSet.
	^(all difference: ok) asArray sort! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'HAW 1/17/2024 20:43:19'!
extraVMMemory
	"Answer the current setting of the 'extraVMMemory' VM parameter. See the comment in extraVMMemory: for details."

	^ self vmParameterAt: 23
! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'HAW 1/17/2024 20:43:24'!
extraVMMemory: extraBytesToReserve
	"Request that the given amount of extra memory be reserved for use by the virtual machine to leave extra C heap space available for things like plugins, network and file buffers, and so on. This request is stored when the image is saved and honored when the image is next started up. Answer the previous value of this parameter."

	extraBytesToReserve < 0
		ifTrue: [self error: 'VM memory reservation must be non-negative'].
	^ self vmParameterAt: 23 put: extraBytesToReserve
! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'HAW 1/17/2024 20:43:47'!
getCurrentWorkingDirectory
	"Do our best effort to answer the path from wich Cuis was started.

	Smalltalk getCurrentWorkingDirectory


On Linux, starting as
	juan@juandebian:/media/sf_SharedWithLinux/testPayload03/payload$      coglinux/bin/squeak        Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image
we get:
	Smalltalk vmPath                       					'/media/sf_SharedWithLinux/testPayload03/payload/coglinux/lib/squeak/4.5-3370/'
	Smalltalk imagePath   							'/media/sf_SharedWithLinux/testPayload03/payload/Cuis-Smalltalk-Dev-master'
	Smalltalk primGetCurrentWorkingDirectoryUnix    '/media/sf_SharedWithLinux/testPayload03/payload'
	Smalltalk getCurrentWorkingDirectory 			'/media/sf_SharedWithLinux/testPayload03/payload'


On Windows 7, starting as
	C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload>        cogwin\squeak.exe            Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image
we get:
	Smalltalk vmPath    										'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\cogwin\'
	Smalltalk imagePath   									'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master'
	Smalltalk primGetCurrentWorkingDirectoryWindows       	'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master'
	Smalltalk getCurrentWorkingDirectory  					'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload'

	Easy on Unix. Harder on Windows, because #primGetCurrentWorkingDirectoryWindows insists on lying and answers the directory containing the image.
	"
	
	| fullImagePathName imageSpecCmdLineArgument |

	"Easy on Unix"
	 self primGetCurrentWorkingDirectoryUnix ifNotNil: [ :cwd |
		"It seems on Mac, when dropping image on VM, we get '/', that is not really meaningful"
		(cwd = '/' and: [ self platformName = 'Mac OS' ]) ifFalse: [
			^ cwd ]].

	"On Windows, extract shell path if image is in subtree"
	fullImagePathName := self imageName.
	imageSpecCmdLineArgument := self getSystemAttribute: 1.
	imageSpecCmdLineArgument ifNotNil: [
		(imageSpecCmdLineArgument size < fullImagePathName size and: [			"Not if they are equal, as if they are both fully qualified (absolute) paths"
			fullImagePathName asPathTokens endsWith: imageSpecCmdLineArgument asPathTokens]) ifTrue: [
				^ fullImagePathName copyFrom: 1 to: fullImagePathName size - imageSpecCmdLineArgument size - 1 ]].

	"If we don't know better, answer nil. If appropriate, senders do:
		Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]
	to have, at least, some valid directory
	"
	^ nil! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'HAW 1/17/2024 20:44:30'!
isDevelopmentEnvironmentPresent
	"Or we can't open a Smalltalk debugger"

	^ self isHeadless not and: [self includesKey: #Debugger]! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'HAW 1/17/2024 20:52:51'!
vmOptionsDo: aBlock
	"Repeatedly evaluate aBlock for each vm option specified by the commandline that started Cuis.
	aBlock has two arguments: the vm option itself and the index (position)
	
	Smalltalk vmOptionsDo: [ :option :i | {i. option} print ]
	"
	| i vmOption |
	i := -1.
	[vmOption := self getSystemAttribute: i.
	vmOption notNil ] whileTrue: [
		aBlock value: vmOption value: i.
		i := i-1 ]! !

!SystemDictionary methodsFor: 'printing' stamp: 'jmv 9/3/2012 18:04'!
printOn: aStream
	self == Smalltalk
		ifTrue: [ aStream nextPutAll: 'Smalltalk' ]
		ifFalse: [ super printOn: aStream ]! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 1/17/2024 20:35:34'!
allBehaviorsDo: aBlock 
	"Evaluate the argument, aBlock, for each kind of Behavior in the system 
	(that is, Object and its subclasses).
	ar 7/15/1999: The code below will not enumerate any obsolete or anonymous
	behaviors for which the following should be executed:

		Smalltalk allObjectsDo:[:obj| obj isBehavior ifTrue:[aBlock value: obj]].

	but what follows is way faster than enumerating all objects."

	self do: [ :root |
		(root isBehavior and: [root superclass isNil]) ifTrue: [	"Grab ProtoObject and any other alike"
			root withAllSubclassesDo: [ :class |
				class isMeta ifFalse: [ "The metaclasses are rooted at Class; don't include them twice."
					aBlock
						value: class;
						value: class class ]]]]! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 1/17/2024 20:36:20'!
allMethodsSourceStringMatching: aString
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.
	Search the class comments also.
	Argument might include $*, that matches any subsequence.
	For example, try:
		ensure:*[*close*]
	"
	| list classCount adder |
	list := Set new.
	adder := [ :mrClass :mrSel |
		list add: (MethodReference class: mrClass selector: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self classNames size
		during: [ :barBlock |
			classCount := 0.
			self allClassesDo: [ :class |
				barBlock value: (classCount := classCount + 1).
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						(aString match: (cl sourceCodeAt: sel)) ifTrue: [
							adder
								value: cl
								value: sel ]].

					(aString match: cl organization classComment) ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 1/17/2024 20:38:01'!
allMethodsWithSourceString: aString matchCase: caseSensitive
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.  Search the class comments also"
	| list classCount adder |
	list := Set new.
	adder := [ :mrClass :mrSel |
		list add: (MethodReference class: mrClass selector: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self classNames size
		during: [ :barBlock |
			classCount := 0.
			self allClassesDo: [ :class |
				barBlock value: (classCount := classCount + 1).
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						((cl sourceCodeAt: sel)
							findString: aString
							startingAt: 1
							caseSensitive: caseSensitive) > 0 ifTrue: [
							adder
								value: cl
								value: sel ]].
					(cl organization classComment
						findString: aString
						startingAt: 1
						caseSensitive: caseSensitive) > 0 ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 1/17/2024 20:38:05'!
allReferencesToLiteral: aLiteral
	| coll |
	coll := OrderedCollection new.
	self allBehaviorsDo: [ :eaClass |
		eaClass
			addMethodsTo: coll
			thatReferenceTo: aLiteral
			special: false
			byte: nil ].
	^ coll.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 1/17/2024 20:38:14'!
allSentMessagesWithout: classesAndMessagesPair
	"Answer the set of selectors which are sent somewhere in the system, 
	computed in the absence of the supplied classes and messages."

	| sent absentClasses absentSelectors |
	sent := IdentitySet new: CompiledMethod instanceCount.
	absentClasses := classesAndMessagesPair first.
	absentSelectors := classesAndMessagesPair second.
	self classNames do: [ :cName | 
		((absentClasses includes: cName)
				ifTrue: [#()]
				ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
			 (absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				 "Include all sels, but not if sent by self"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m |
					m == sel ifFalse: [sent add: m] ]]]].
		"The following may be sent without being in any literal frame"
		1 to: self specialSelectorSize do: [ :index | 
			sent add: (self specialSelectorAt: index)].
	self presumedSentMessages do: [ :sel | sent add: sel].
	^ sent! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 1/17/2024 20:38:48'!
allUnusedClassesWithout: classesAndMessagesPair
	"Enumerates all classes in the system and returns a list of those that are apparently unused. A class is considered in use if it (a) has subclasses or (b) is referred to by some method or (c) has its name in use as a literal (but not in the same class) or: (d) some instance is a global."
	"
	Smalltalk unusedClasses
	"

	| unused cl |
	self garbageCollect.
	unused := self classNames asIdentitySet
				copyWithoutAll: (self allGlobalRefsWithout: classesAndMessagesPair).
				
	self do: [ :global |
		unused remove: global class name ifAbsent: nil].
	
	^ unused reject: [ :cName |
		cl := self at: cName.
		cl subclasses notEmpty
			"or: [ cl someInstance notNil ]"
			"or: [cl inheritsFrom: FileDirectory]"]! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 1/17/2024 20:46:14'!
poolUsers
	"Answer a dictionary of pool name -> classes that refer to it. Also includes any globally know dictionaries (such as Smalltalk, Undeclared etc) which although not strictly accurate is potentially useful information "
	"Smalltalk poolUsers"
	| poolUsers pool refs |
	poolUsers := Dictionary new.
	self keys
		do: [ :k |
			 (((pool := self at: k) is: #Dictionary)
					or: [pool isKindOf: SharedPool class])
				ifTrue: [refs := self allClasses
								select: [:c | c sharedPools identityIncludes: pool]
								thenCollect: [:c | c name].
					refs := refs asOrderedCollection.
					refs
						add: (self
								allCallsOn: (self associationAt: k)).
					poolUsers at: k put: refs]].
	^ poolUsers! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'HAW 1/17/2024 20:35:27'!
abandonSources
	"
	Smalltalk abandonSources
	"
	 | m bTotal bCount |
	(self confirm:
'This method will detach the image fom source code.
A fresh changes file will be created to record further changes.
-- CAUTION --
If you have backed up your system and
are prepared to face the consequences of
abandoning source code files, choose Yes.
If you have any doubts, you may choose No
to back out with no harm done.')
		==  true ifFalse: [^ self inform: 'Okay - no harm done'].
	bTotal := 0.  bCount := 0.
	self allBehaviorsDo: [: b | bTotal := bTotal + 1].
'Doing #destroySourcePointer ...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: bTotal
	during: [ :barBlock |
		self allBehaviorsDo: [ :cl |
		"for testing"
		"{ EllipseMorph } do: [ :cl |"
			barBlock value: (bCount := bCount + 1).
			cl selectors do: [:selector |
				m := cl compiledMethodAt: selector.
				m destroySourcePointer ]]].
	self allBehaviorsDo: [:b | b zapOrganization].
	self closeSourceFiles.
	Preferences at: #warnIfNoChangesFile put: false.
	Preferences at: #warnIfNoSourcesFile put: false! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'HAW 1/17/2024 20:48:51'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	self at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	WorldMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w submorphsDo: [ :a | a delete ]].
	Preferences at: #wantsMenuIcons put: false.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	BitBltCanvas releaseClassCachedState.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	self garbageCollect.

	self removeEmptyMessageCategories.
	self organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally DynamicTypingSmalltalkCompleter).
	AppLauncher appGlobalName ifNotNil: [ :any |
		keep add: any ].
	unused := self unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(self at: c) removeFromSystem]. 
		n := self removeAllUnSentMessages.
		unused := self unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	self garbageCollect.

	self removeEmptyMessageCategories.
	self organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	self garbageCollect.
	oldDicts := MethodDictionary allInstances.
	newDicts := Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts := newDicts := nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'HAW 1/17/2024 20:49:00'!
removeAllUnSentMessages
	"Smalltalk removeAllUnSentMessages"
	"[Smalltalk unusedClasses do: [:c | (Smalltalk at: c) removeFromSystem]. 
	Smalltalk removeAllUnSentMessages > 0] whileTrue."
	"Remove all implementations of unsent messages."
	| sels n |
	sels := self allUnSentMessages.
	self presumedSentMessages
		do: [:sel | sels
				remove: sel
				ifAbsent: nil].
	sels isEmpty
		ifTrue: [^ 0].
	n := 0.
	self
		allBehaviorsDo: [:x | n := n + 1].
	'Removing ' , sels size printString , ' messages . . .'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: n
		during: [:barBlock | 
			n := 0.
			self
				allBehaviorsDo: [:class | 
					barBlock value: (n := n + 1).
					sels
						do: [:sel | class removeSelector: sel]]].
	^ sels size! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'HAW 1/17/2024 20:49:24'!
removeSelector: descriptor
	"Safely remove a selector from a class (or metaclass). If the class
	or the method doesn't exist anymore, never mind and answer nil.
	This method should be used instead of 'Class removeSelector: #method'
	to omit global class references."

	| class sel |
	class := self at: descriptor first ifAbsent: [^ nil].
	(descriptor size > 2 and: [descriptor second == #class])
		ifTrue:
			[class := class class.
			sel := descriptor third]
		ifFalse: [sel := descriptor second].
	^ class removeSelector: sel! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'HAW 1/17/2024 20:49:55'!
removedUnusedClassesAndMethods
	[
		#hereWeGo print.
		self unusedClasses do: [:c | 
			c print.
			(self at: c) removeFromSystem]. 
		self removeAllUnSentMessages > 0 or: [ self unusedClasses notEmpty ]] whileTrue.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 1/17/2024 20:43:35'!
filenameForUpdatedImage: aBoolean
	"If aBoolean, include highest update number	"
	| fileName newName changesName systemVersion |
	systemVersion := SystemVersion current.
	fileName := String streamContents: [ :strm |
		strm nextPutAll: systemVersion versionString.
		aBoolean ifTrue: [
			strm
				nextPut: $-;
				print: systemVersion highestUpdate ].
		self isSpur
			ifTrue: [	
				self wordSize = 4 ifTrue: [
					strm nextPutAll: '-32' ]]
			ifFalse: [
				strm nextPutAll: '-v3' ]].
	newName := fileName, '.image'.
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName := DirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName := self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFileEntry exists ifTrue: [
		self inform:
'There is already .changes file of the desired name,
', changesName, '
curiously already present, even though there is no
corresponding .image file.
Please remedy manually and then repeat your request.'.
		^nil  ].
	^newName! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 1/17/2024 20:46:58'!
printStuffToCleanOnImageSave
	"So far, to serve experiments.
	Some day, when this is empty, we'll be closer to bootstrap from sources."
	| n nonNilVars m classInstVars v|
	n := 0.
	""
	'--------' print.
	'Proceeses: ' print.
	(Processor processes
		sort: [ :a :b | a priority >= b priority ]) do: [ :p | p print ].
	'======' print.
	'Globals: ' print.
	self globals associationsDo: [ :g | " {g key. g value class }" g  print ].
	'======' print.
	'ClassVars (notNil):  ' print.
	n := 0.
	self hierachySortedAllClassesDo: [ :cls |
		"Cleared, but come again for lazy init during this report.
		therefore, not required for bootstrap from sources"
		({ Scanner } includes: cls) ifFalse: [
			nonNilVars := cls classPool select: [ :var | var notNil ].
			nonNilVars notEmpty ifTrue: [
				n := n + 1.
				{ cls. nonNilVars keys} print ]]].
	'======' print.
	'InstClassVars (notNil): ' print.
	m := 0.
	self hierachySortedAllClassesDo: [ :cls |
		classInstVars := Dictionary new.
		cls class allRegularInstVarNames do: [ :nam |
			v := cls instVarNamed: nam.
			v ifNotNil: [
				classInstVars at: nam put: v ]].
		classInstVars notEmpty ifTrue: [
			m := m + 1.
			{ cls class. classInstVars keys }  print ]].
	'======' print.
	
	{ n. m}  print! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 1/17/2024 20:50:30'!
saveNumberedRelease
	"Save the image/changes using the next release version number."
	"
	Smalltalk saveNumberedRelease
	"
	| versionMajor versionMinor previousValue |
	versionMajor := 6.
	versionMinor := 2.
	SystemVersion newVersionMajor: versionMajor versionMinor: versionMinor.
	"While condensing sources, convert all variable assignment to ANSI Smalltalk form, i.e. :="
	previousValue := Preferences at: #fileOutANSIassignment.
	Preferences at: #fileOutANSIassignment put: true.
	self condenseSources: SystemVersion current versionString.
	Preferences at: #fileOutANSIassignment put: previousValue.
	self setDefaultGCParameters.

	(self filenameForUpdatedImage: false) ifNotNil: [ :newName |
		SystemVersion current beNumberedRelease.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndStayAs: newName clearAllClassState: true ].! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 1/17/2024 20:50:35'!
setDefaultGCParameters
	"Adjust the VM's default GC parameters to avoid premature tenuring, and too frequent scavenging.
	Parameters set here persist in saved images, so we set them image save for release.
	See #setGCParameters"

	"Desired Eden size: "
	self vmParameterAt: 45 put: `16*1024*1024`.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 1/17/2024 20:50:53'!
setGCParameters
	"Adjust the VM's default GC parameters to avoid premature tenuring, and too frequent scavenging.
	Parameters set here don't persist, so we set them on startup.
	See #setDefaultGCParameters"

	"Grow old memory in chunks of: "
	self vmParameterAt: 25 put: `32*1024*1024`.
	"Shrink heap when unused memory is at least: "
	self vmParameterAt: 24 put: `64*1024*1024`.

	self isSpur
		ifTrue: [
			"Note: (jmv, 9/2021) It seems modern Spur VMs ignore parameter 6. This all seems to be bogus."
			| proportion edenSize survivorSize averageObjectSize numObjects |
			proportion := 0.9. "tenure when 90% of pastSpace is full"
			edenSize := self vmParameterAt: 44.
			survivorSize := edenSize / 5.0. "David's paper uses 140Kb eden + 2 x 28kb survivor spaces; Spur uses the same ratios :-)"
			averageObjectSize := 8 * self wordSize. "a good approximation"
			numObjects := (proportion * survivorSize / averageObjectSize) rounded.
			self vmParameterAt: 6 put: numObjects.  "tenure when more than this many objects survive the GC"
			"/Note: (jmv, 9/2021)"
			
			"Do a full GC when used memory grows by this factor. Fails on non-Spur VMs.
			Default is 0.333.
			2.0 means gull GC when heap size triples."
			self vmParameterAt: 55 put: 2.0.
			]
		ifFalse: [
			self vmParameterAt: 5 put: 4000.  "do an incremental GC after this many allocations"
			self vmParameterAt: 6 put: 2000.  "tenure when more than this many objects survive the GC"
			].! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'HAW 1/17/2024 20:51:46'!
snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: clearAllStateFlag
	"This is the main method for image save and / or quit.
	See senders."
	"WARNING: Current process will be killed. UI Process will be restarted"
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic
	checks for a document file to process when starting up."
	"
	To test the full cleanup and startup procedures, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: true

	To test the cleanup done when saving the image, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: false
	"
	| activeProc |
	activeProc := Processor activeProcess.
[
	| isARealStartup guiRootObject guiRootObjectClass |
	save not & quit
		ifTrue: [ (SourceFiles at: 2) ifNotNil: [ :changes |
				ChangesInitialFileSize ifNotNil: [ changes truncate: ChangesInitialFileSize ]]]
		ifFalse: [ self logSnapshot: save andQuit: quit ].
	clearAllStateFlag ifTrue: [
		TranscriptWindow allInstancesDo: [ :each |
			each isInWorld ifTrue: [ each delete.]].
		UISupervisor ui tearDownDesktop.
		Transcript logToFile: false ].
	ActiveModel flushEventSystem.
	self processShutDownList: quit.
	"These could be moved to some #shutDown"
	self stopLowSpaceWatcher.
	WeakArray stopFinalizationProcess.
	ProcessorScheduler stopBackgroundProcess.
	"Stuff needed to relaunch UI on startup"
	guiRootObjectClass := UISupervisor ui class.
	"Replace with this to create a new world at startup after 'saveAsNewVersion'"
	guiRootObject := clearAllStateFlag ifFalse: [ UISupervisor ui ].

	UISupervisor stopUIProcess.
	activeProc isTerminated ifFalse: [ activeProc terminate ].
	guiRootObject ifNotNil: [ guiRootObject releaseCachedState ].
	clearAllStateFlag ifTrue: [
		Processor processesDo: [ :p |
			(p name = 'Formerly: Morphic UI') ifTrue: [ p terminate ]]].
	MorphicCanvas allSubInstancesDo: [ :c |
		(c ownerProcess notNil and: [c ownerProcess isTerminated]) ifTrue: [
			c releaseEngine			]].

	"Clean Globals"
	self at: #Sensor put: nil.
	self closeSourceFiles.
	self at: #SourceFiles put: nil.
	self allClassesDo: [ :cls |
		cls releaseClassCachedState ].
	clearAllStateFlag ifTrue: [
		self allClassesDo: [ :cls |
			cls releaseClassState ]].
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [
		Transcript clear.
		"Enable this call to actually see the image clean report."
		false ifTrue: [self printStuffToCleanOnImageSave]].
	"Do image save & quit as apropriate"
	(Cursor cursorAt: #writeCursor) activateCursor.
	save
		ifTrue: [
			"The snapshot primitive answers false if it was just called to do the snapshot.
			But image startup is resumed by returning (again) from the primitive, but this time answering true."
			isARealStartup := embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ isARealStartup := false ].
	quit & (isARealStartup == false) ifTrue: [ self quitPrimitive ].

	"If starting from absolute scratch, this would be a good time to recreate Global names"
	self at: #Sensor put: nil.
	self at: #SourceFiles put: (Array new: 2).
	self logStartupDebugAid: 'To #openSourceFiles'.
	self openSourceFiles.
	"Here, startup begins!! (isARealStartup might be nil)"
	self logStartupDebugAid: 'To #initClassCachedState'.
	self allClassesDo: [ :cls | cls initClassCachedState ].
	self logStartupDebugAid: 'To #doStartUp:'.
	self doStartUp: isARealStartup == true.
	self logStartupDebugAid: 'To #spawnNewMorphicProcessFor: ', guiRootObject printString.
	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject := guiRootObjectClass newWorld ]).
	self logStartupDebugAid: 'To #restoreLostChangesIfNecessary'.
	self restoreLostChangesIfNecessary.
	clearAllStateFlag ifTrue: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #recreateDefaultDesktop'.
			guiRootObject recreateDefaultDesktop; restoreDisplay ]]
	ifFalse: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #restoreDisplay'.
			guiRootObject restoreDisplay ]].
	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [
		UISupervisor whenUIinSafeState: [
			self logStartupDebugAid: 'To #readAndApplyUserPrefs'.
			self readAndApplyUserPrefs.
			self logStartupDebugAid: 'To #processCommandLineArguments'.
			self processCommandLineArguments.
			self logStartupDebugAid: 'To #launchApp'.
			AppLauncher launchApp ]].
	"Now it's time to raise an error"
	isARealStartup ifNil: [ self logStartupError: 'Failed to write image file (disk full?)' ]
]
	forkAt: Processor timingPriority - 1
	named: 'Startup process'.! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 1/17/2024 20:39:42'!
assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges oldUserChangesName |
	StartupStamp ifNil: [^ self].
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp; newLine.
			self forceChangesToDisk ]].
	(Preferences at: #autoNumberUserChanges) ifTrue: [
		oldUserChanges := DirectoryEntry userChangesDirectory // self userChangesFileName.
		oldUserChanges exists ifTrue: [
			directory := oldUserChanges parent.
			oldUserChangesName := directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
			oldUserChanges rename: oldUserChangesName ]].
	DirectoryEntry userChangesDirectory // self userChangesFileName appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp; newLine.
		stream flush ].
	StartupStamp := nil! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 1/17/2024 20:44:40'!
logChange: aStringOrText preamble: preambleOrNil
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	self assureStartupStampLogged.
	aString := aStringOrText asPlainString.
	aString firstNonSeparator = 0 ifTrue: [^ self].  "null doits confuse replay"
	
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			self forceChangesToDisk ]].
	Utilities logsUserChanges ifTrue: [
		DirectoryEntry userChangesDirectory // self userChangesFileName appendStreamDo: [ :stream |
			stream newLine; newLine.
			preambleOrNil ifNotNil: [
				stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			stream nextChunkPut: aString.
			stream flush ]]! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 1/17/2024 20:45:58'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems.
	Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes entry msg lastQuitLogPosition snapshotChunk entryPathName |

	"Do not open source files if internalized (i.e. notNil)"
	sources := SourceFiles at: 1.
	sources ifNil: [
		entry := self defaultSourcesName asFileEntry.
		entry exists ifFalse: [
			entry := self alternativeSourcesName asFileEntry ].
		entry exists ifTrue: [
			sources := [ entry readStream ] on: FileDoesNotExistException do: [ nil ]]].
	(sources isNil and: [ Preferences at: #warnIfNoSourcesFile ])
		ifTrue: [
				msg := 'Cuis cannot locate the sources file named ' , entry pathName, '.', String newLineString,
					'Please check that the file is properly named and is in the same directory as this image.'.
			self logStartupError: msg.
		].

	"Do not open source files if internalized (i.e. notNil)"
	changes := SourceFiles at: 2.
	changes ifNil: [
		entry := self defaultChangesName asFileEntry.
		entryPathName := entry pathName.
		"Read the last snapshot chunk. Validate that the .changes file is appropriate for this image."
		entry exists
			ifTrue: [
				lastQuitLogPosition := self lastQuitLogPosition.
				lastQuitLogPosition > 0 ifTrue: [
					entry readStreamDo: [ :changesFile |
						changesFile position: lastQuitLogPosition.
						snapshotChunk := changesFile nextChunk ].
					((snapshotChunk beginsWith: self tagHeader) and: [
						snapshotChunk includesSubString: 'priorSource: ']) ifFalse: [
							(Preferences at: #warnIfNoChangesFile) ifTrue: [
								self logStartupError:
									'Incorrect changes file: ', entryPathName, String newLineString,
									'Missing code will be decompiled', String newLineString,
									'New source code will not be saved' ].
							entry := nil ]]]
			ifFalse: [
				(Preferences at: #warnIfNoChangesFile) ifTrue: [
					self logStartupError:
						'Could not find changes file: ', entryPathName, String newLineString,
						'Missing code will be decompiled', String newLineString,
						'New source code will not be saved' ].
				entry := nil ].
		entry ifNotNil: [
			changes := [ entry appendStream ]
				on: FileWriteError
				do: [
					self logStartupError:
						'Could not write to changes file: ', entryPathName, String newLineString,
						'Changes file will not be used.', String newLineString,
						'Missing code will be decompiled', String newLineString,
						'New source code will not be saved'.
					nil ]]].
	ChangesInitialFileSize := changes ifNotNil: [ changes position ].

	SourceFiles := Array with: sources with: changes.! !

!SystemDictionary methodsFor: 'special objects' stamp: 'HAW 1/17/2024 20:42:32'!
compactClassesArray  "Smalltalk compactClassesArray"
	"Return the array of 31 classes whose instances may be
	represented compactly"
	^ self specialObjectsArray at: 29! !

!SystemDictionary methodsFor: 'ui' stamp: 'HAW 1/17/2024 20:39:47'!
beep
	"
	Smalltalk beep
	"
	(Preferences at: #soundsEnabled) ifTrue: [
		self
			at: #SampledSound
			ifPresent: [ :cls | cls beep ]
			ifAbsent: [ self primitiveBeep ]]! !

!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 1/17/2024 20:40:49'!
browseClassCommentsWithString: aString
	"Smalltalk browseClassCommentsWithString: 'my instances' "
	"Launch a message list browser on all class comments containing aString as a substring."
	| caseSensitive suffix list |
	suffix := (caseSensitive := Sensor shiftPressed)
		ifTrue: [ ' (case-sensitive)' ]
		ifFalse: [ ' (use shift for case-sensitive)' ].
	list := Set new.
	self allClassesDo: [ :class |
		(class organization classComment
			findString: aString
			startingAt: 1
			caseSensitive: caseSensitive) > 0 ifTrue: [
			list add: (MethodReference class: class selector: #Comment) ]].
	^ self
		browseMessageList: list
		name: 'Class comments containing ', aString printString, suffix
		autoHighlight: aString
		allOccurrences: true.! !

!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 1/17/2024 20:40:57'!
browseClassesWithNamesContaining: aString caseSensitive: caseSensitive 
	"Smalltalk browseClassesWithNamesContaining: 'eMorph' caseSensitive: true "
	"Launch a class-list list browser on all classes whose names containg aString as a substring."

	| suffix aList |
	suffix := caseSensitive
				ifTrue: [' (case-sensitive)']
				ifFalse: [' (use shift for case-sensitive)'].
	aList := OrderedCollection new.
	self allClassesDo: [ :class |
		(class name includesSubstring: aString caseSensitive: caseSensitive)
			ifTrue: [aList add: class name]].
	aList notEmpty
		ifTrue: [HierarchyBrowserWindow forClassesNamed: aList asSet sorted title: 'Classes whose names contain ' , aString , suffix]! !

!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 1/17/2024 20:41:13'!
browseEqSmallConstant

    "Dan Ingalls' search for arithmetic use of ==
Answer whether the receiver contains the pattern <expression> == <constant>,
where constant is -1, 0, 1, or 2...
	Smalltalk browseEqSmallConstant
"

	self browseAllSelect: [:m | m scanForEqSmallConstant]! !

!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 1/17/2024 20:41:18'!
browseLikelyUnneededRedefinitions

	"
	Spot unneeded method redefinitions: methods that are equal to what would be inherited if they weren't there
	Smalltalk browseLikelyUnneededRedefinitions
	Be careful with class side #initialize methods that set up instance class variables
	In general, class side #initialize methods can't be removed unless empty.
	"

	self browseAllSelect: [ :cm |
		(cm methodClass superclass ifNotNil: [ :sup | sup lookupSelector: cm selector]) = cm ]! !

!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 1/17/2024 20:41:26'!
browseObsoleteReferences   "Smalltalk browseObsoleteReferences"
	| references |
	references := OrderedCollection new.
	(LookupKey allSubInstances select:
		[:x | ((x value isKindOf: Behavior) and: ['AnOb*' match: x value name]) or:
		['AnOb*' match: x value class name]]) 
		do: [:x | references addAll: (self allCallsOn: x)].
	self 
		browseMessageList: references 
		name: 'References to Obsolete Classes'! !

!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 1/17/2024 20:41:53'!
browseViewReferencesFromNonViews
	"
	Smalltalk browseViewReferencesFromNonViews
	"
	| aLiteral aCollection |

	aCollection := OrderedCollection new.

	"Tweak to look just for pluggables or also for menus (or maybe for all morphs)"
"	PopUpMenu withAllSubclasses , MenuMorph withAllSubclasses , PluggableMorph withAllSubclasses do: [ :view |"
	PluggableMorph withAllSubclassesDo: [ :view |
"	MenuMorph withAllSubclassesDo: [ :view |"
		
		aLiteral := view name.
		
		"tweak to linclude refs to SysWindow subhierarchy or not"
		(view includesBehavior: SystemWindow) & false ifFalse: [
			self allBehaviorsDo: [ :class |
				((class includesBehavior: Morph) or: [ class includesBehavior: Morph class ]) ifFalse: [
					class addMethodsTo: aCollection thatReferenceTo: aLiteral special: false byte: nil ]]]].
	
	self
		browseMessageList: aCollection asSet
		name: 'References to Views from non-Views'.! !

!SystemDictionary methodsFor: 'image format' stamp: 'HAW 1/17/2024 20:43:58'!
imageFormatVersion
	"Answer an integer identifying the type of image in memory. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). This invokes an optional primitive
	that may not be available on all virtual machines.
	Answer nil if unknown."

	"
	Smalltalk imageFormatVersion
	"

	<primitive: 'primitiveImageFormatVersion'>

	"Cog provides a VM parameter"
	^self vmParameterAt: 41! !

!SystemDictionary methodsFor: 'image format' stamp: 'HAW 1/17/2024 20:44:08'!
imageFormatVersionFromFileAsIs
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). If the image file has a different
	endianness than the VM, the format version will appear byte-swapped."
	"
	Smalltalk imageFormatVersionFromFileAsIs
	"
	^ self imageName asFileEntry readStreamDo: [ :stream |
		(stream binary; next: 4)
			uint32At: 1
			bigEndian: self isBigEndian ]! !

!SystemDictionary methodsFor: 'startup' stamp: 'HAW 1/17/2024 20:47:13'!
processCommandLineOption: optionString optionArgument: optionArgument raiseExceptions: aBoolean
	"
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -h
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -v
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -q
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -e -d''0 zope''
Squeak.app/Contents/MacOS/Squeak Cuis-Smalltalk-Dev/Cuis6.0-????.image -r YAXO -rJSON -d ''Smalltalk startUpScriptArguments print'' xx YY 3492 -d'' 'here comes another' print'' -d ''{'The answer is: '. Smalltalk startUpScriptArguments first asNumber + Smalltalk startUpScriptArguments second asNumber} print'' 3 4 -- -d'' 'not this' print' ''

Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -r RequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d ''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st param1ToScript param2ToSCript ''parametro 3 to script ->>>--// 3''
	"
	"You can use 'Smalltalk startUpScriptArguments' inside the startUp script:"


	| exceptionToIgnoreOrNil entry |
	exceptionToIgnoreOrNil := aBoolean ifFalse: [UnhandledError] ifTrue: [nil].
	optionString caseOf: {
		[ '-h' ] -> [
			self displayCommandLineUsageOn: StdIOWriteStream stdout.
			self quit ].

		[ '-v' ] -> [ 
			self displaySystemInformationStringOn: StdIOWriteStream stdout.
			self quit ].

		[ '-q' ] -> [ self quit ].

		[ '-r' ] -> [		"as in 		-rRequiredFeature2        or         -r RequiredFeature2"
			{ 'Feature require: '. optionArgument } print.
			[ Feature require: optionArgument ] on: exceptionToIgnoreOrNil do: [:ex | ex return] ].

		[ '-d' ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
			{ 'Compiler evaluate: '. optionArgument } print.
			[ Compiler evaluate: optionArgument ] on: exceptionToIgnoreOrNil do: [:ex | ex return] ].

		['-l' ] -> ["file in the file"
			{ 'File in: '. optionArgument} print.
			[(CodeFile newFromFile: optionArgument asFileEntry) fileIn] on: exceptionToIgnoreOrNil do: [:ex | ex return]].

		[ '-s' ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parameter to script script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
			{ 'Compiler evaluate contents of file named: '. optionArgument. ' arguments: '. self startUpScriptArguments } print.
			entry := optionArgument asFileEntry.
			entry exists ifTrue: [
				entry readStreamDo: [ :stream |
					[ Compiler evaluate: stream contentsOfEntireFile ] on: exceptionToIgnoreOrNil do: [:ex | ex return]]]].

		[ '-ud' ] -> [ DirectoryEntry setUserBaseDirectory: optionArgument asDirectoryEntry ].

		[ '-udIsBase' ] -> [ DirectoryEntry setUserBaseDirectoryAsCuisBase ].
	}
	otherwise: [].! !

!SystemDictionary methodsFor: 'startup' stamp: 'HAW 1/17/2024 20:50:59'!
setStartupStamp

	| dateAndTime |
	dateAndTime := DateAndTime now.
	StartupStamp := String streamContents: [ :stream |
		stream nextPutAll: '----STARTUP---- ('.
		dateAndTime date printOn: stream.
		stream space.
		dateAndTime time print24: true showSeconds: true on: stream.
		stream
			nextPutAll: ') as ';
			nextPutAll: self imageName ] 
! !

!SystemDictionary methodsFor: 'vm parameters' stamp: 'HAW 1/17/2024 20:43:13'!
doMixedArithmetic
	"If true, then primitives can handle the conversions:
	SmallInteger arithmeticOp: Float (Small or Boxed)
	SmallInteger compareOp: Float (Small or Boxed)
	Else, the primitive fail in case of mixed arithmetic, and conversion will be performed at image side.
	See doMixedArithmetic:
	
	Note:
		OpenSmalltalk VMs after March, 2019 can set the option and will honor it. The comparison operation behaves as if the Float was converted #asTrueFraction. This means that some rather big SmallIntegers in 64 bit systems, that can not be represented exactly as a Float will not be equal to any Float. Squeak adopted this critera. Cuis follows the more conventional, Smalltalk-80 tradition to always convert to Float if any operand is Float. Therefore Cuis needs to do 'Smalltalk doMixedArithmetic: false'.
		Previous VMs can not set the option, and will answer true when queried. But these VMs did the conversion to Float, and the requested operation in Floats. So, with these VMs, Cuis will also have the desired behavior."

	^ ((self vmParameterAt: 48) allMask: 64) not! !


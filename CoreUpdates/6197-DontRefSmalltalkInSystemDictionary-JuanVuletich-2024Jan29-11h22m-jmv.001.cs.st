'From Cuis6.3 [latest update: #6196] on 29 January 2024 at 11:32:37 am'!

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 1/29/2024 11:28:11'!
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
   Smalltalk browseAllUnimplementedCalls"! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/29/2024 11:26:08'!
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
		self wordSize = 4 ifTrue: [
			strm nextPutAll: '-32' ]].
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

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/29/2024 11:28:17'!
setGCParameters
	"Adjust the VM's default GC parameters to avoid premature tenuring, and too frequent scavenging.
	Parameters set here don't persist, so we set them on startup.
	See #setDefaultGCParameters"

	| proportion edenSize survivorSize averageObjectSize numObjects |

	"Grow old memory in chunks of: "
	self vmParameterAt: 25 put: `32*1024*1024`.
	"Shrink heap when unused memory is at least: "
	self vmParameterAt: 24 put: `64*1024*1024`.

	"Note: (jmv, 9/2021) It seems modern Spur VMs ignore parameter 6. This all seems to be bogus."
	proportion := 0.9. "tenure when 90% of pastSpace is full"
	edenSize := self vmParameterAt: 44.
	survivorSize := edenSize / 5.0. "David's paper uses 140Kb eden + 2 x 28kb survivor spaces; Spur uses the same ratios :-)"
	averageObjectSize := 8 * self wordSize. "a good approximation"
	numObjects := (proportion * survivorSize / averageObjectSize) rounded.
	self vmParameterAt: 6 put: numObjects.  "tenure when more than this many objects survive the GC"
	"/Note: (jmv, 9/2021)"
	
	"Do a full GC when used memory grows by this factor.
	Default is 0.333.
	2.0 means gull GC when heap size triples."
	self vmParameterAt: 55 put: 2.0.! !


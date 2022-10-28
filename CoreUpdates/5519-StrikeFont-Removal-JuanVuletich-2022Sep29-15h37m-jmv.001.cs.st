'From Cuis 6.0 [latest update: #5494] on 29 September 2022 at 3:40:53 pm'!

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 9/22/2022 14:52:45'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
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

	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally DynamicTypingSmalltalkCompleter).
	AppLauncher appGlobalName ifNotNil: [ :any |
		keep add: any ].
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

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


!Preference class methodsFor: 'helpers' stamp: 'jmv 9/22/2022 14:52:35'!
detectType: anObject
"When the type is not provided, we can try to deduce it "

	(anObject isKindOf: CharacterSequence) ifTrue: [ ^CharacterSequence ].
	^ anObject class name
		caseOf: {
			[#True] -> [Boolean].
			[#False] -> [Boolean].
			[#TrueTypeFont ] -> [AbstractFont]
		}
		otherwise: [anObject class]
! !

!classRemoval: #StrikeFont stamp: 'jmv 9/29/2022 15:40:39'!
Smalltalk removeClassNamed: #StrikeFont!

!classRemoval: #StrikeFontFamily stamp: 'jmv 9/29/2022 15:40:07'!
Smalltalk removeClassNamed: #StrikeFontFamily!


'From Cuis 5.0 of 7 November 2016 [latest update: #3726] on 5 May 2019 at 11:12:11 am'!

!FontFamily methodsFor: 'accessing' stamp: 'jmv 5/5/2019 11:03:44'!
aroundPointSize: aNumber
	^baseFontBySizes at: aNumber ifAbsent: [ |found |
		found _ nil.
		baseFontBySizes do: [ :font |
			(found isNil or: [ (found pointSize - aNumber) abs > (font pointSize - aNumber) abs ])
				ifTrue: [ found _ font ]].
			found
		]! !

!FontFamily methodsFor: 'accessing' stamp: 'jmv 5/5/2019 11:08:04'!
pointSizes
	^baseFontBySizes keys sort! !


!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 5/5/2019 11:10:37'!
defaultFamilyAndPointSize
	"
	FontFamily defaultFamilyAndPointSize
	"
	| family |
	family _ AvailableFamilies at: DefaultFamilyName.
	^family atPointSize: DefaultPointSize ! !

!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 5/5/2019 11:04:13'!
familyName: aString aroundPointSize: aNumber
	"
	FontFamily familyName: 'DejaVu' aroundPointSize: 120
	"
	| family |
	family _ AvailableFamilies at: aString ifAbsent: [^nil].
	^family aroundPointSize: aNumber! !

!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 5/5/2019 11:08:33'!
pointSizesFor: aString
	"
	FontFamily pointSizesFor: 'DejaVu'
	"
	| family |
	family _ AvailableFamilies at: aString ifAbsent: [^nil].
	^family pointSizes! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 5/5/2019 11:09:34'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	PasteUpMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoMenuIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	BitBltCanvas releaseClassCachedState.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally).
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


!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 5/5/2019 11:11:03'!
default
	"
	Compatibility.
	AbstractFont default
	"
	false ifTrue: [ self deprecatedMethod ]. "Too much senders to activate warnings right now!!"
	^FontFamily defaultFamilyAndPointSize! !

!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 5/5/2019 11:04:58'!
familyName: aString aroundPointSize: aNumber
	"
	Compatibility.
	AbstractFont familyName: Preferences defaultFontFamily aroundPointSize: 120
	"
	false ifTrue: [ self deprecatedMethod ]. "Too much senders to activate warnings right now!!"
	^FontFamily familyName: aString aroundPointSize: aNumber! !

!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 5/5/2019 11:01:15'!
familyName: aString pointSize: aNumber
	"
	Compatibility.
	AbstractFont familyName: Preferences defaultFontFamily pointSize: 12
	"
	false ifTrue: [ self deprecatedMethod ]. "Too much senders to activate warnings right now!!"
	^FontFamily familyName: aString pointSize: aNumber! !

!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 5/5/2019 11:06:50'!
familyNames
	"
	Compatibility.
	AbstractFont familyNames
	"
	false ifTrue: [ self deprecatedMethod ]. "Too much senders to activate warnings right now!!"
	^FontFamily familyNames! !

!AbstractFont class methodsFor: 'instance accessing' stamp: 'jmv 5/5/2019 11:09:11'!
pointSizesFor: aString
	"
	Compatibility.
	AbstractFont pointSizesFor: Preferences defaultFontFamily
	"
	false ifTrue: [ self deprecatedMethod ]. "Too much senders to activate warnings right now!!"
	^FontFamily pointSizesFor: aString! !


!FontFamily methodsFor: 'accessing' stamp: 'jmv 5/5/2019 10:33:56'!
atPointSize: aNumber
	^baseFontBySizes at: aNumber ifAbsent: nil! !

!methodRemoval: StrikeFont class #removeForPDA!
StrikeFont class removeSelector: #removeForPDA!
!methodRemoval: StrikeFont class #removeMostFonts!
StrikeFont class removeSelector: #removeMostFonts!
!methodRemoval: StrikeFont class #removeSomeFonts!
StrikeFont class removeSelector: #removeSomeFonts!
!methodRemoval: AbstractFont class #initialize!
AbstractFont class removeSelector: #initialize!
AbstractFont initialize!

!FontFamily reorganize!
('initialization' atPointSize:put: familyName:)
('accessing' aroundPointSize: atPointSize: familyName pointSizes)
!


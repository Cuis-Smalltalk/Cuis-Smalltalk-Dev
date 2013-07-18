'From Cuis 4.1 of 12 December 2012 [latest update: #1739] on 25 June 2013 at 11:37:44.973955 pm'!

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/25/2013 23:37'!
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
	Preferences useNoIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	FormCanvas clearFormsCache.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
"	ColorPickerMorph class removeSelector: #buildEyedropperIcon."
	CursorWithAlpha class removeSelector: #buildBiggerNormal.
	Theme removeSelector: #miscellaneousIcons.
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont removeMostFonts.
	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally).
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


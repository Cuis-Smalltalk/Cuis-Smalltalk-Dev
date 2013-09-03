'From Cuis 4.2 of 25 July 2013 [latest update: #1817] on 3 September 2013 at 4:12:20.257858 pm'!

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 9/3/2013 12:00'!
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


!CursorWithAlpha class methodsFor: 'constants' stamp: 'jmv 9/3/2013 16:11'!
biggerNormal
	"
	CursorWithAlpha biggerNormal show
	"

	^self constants at: #biggerNormal ifAbsentPut: [
		| form cursor packName pack |
		packName _ #GUIFormsPack.
		pack _ (Smalltalk at: packName) new.
		form _ (pack decodeContentFrom: pack contentMap) at: 'biggerNormalCursor'.
		cursor _ CursorWithAlpha extent: form extent depth: 32.
		form displayOn: cursor.
		cursor offset: -2@-1.
		cursor preMultiplyAlpha.
		cursor ]! !


!FormCanvas class methodsFor: 'cached arrow forms' stamp: 'jmv 9/3/2013 16:11'!
arrowWithGradientOfDirection: aSymbol
	"
	(FormCanvas arrowWithGradientOfDirection: #up) display.
	(FormCanvas arrowWithGradientOfDirection: #down) display.
	(FormCanvas arrowWithGradientOfDirection: #left) display.
	(FormCanvas arrowWithGradientOfDirection: #right) display.
	"
	^CachedForms
		at: { aSymbol . #gradient }
		ifAbsentPut: [ | packName pack |
			packName _ #GUIFormsPack.
			pack _ (Smalltalk at: packName) new.
			(pack decodeContentFrom: pack contentMap)
				at: 'gradient', aSymbol capitalized, 'Arrow' ]! !

!methodRemoval: FormCanvas class #buildArrowWithGradientOfDirection:!
FormCanvas class removeSelector: #buildArrowWithGradientOfDirection:!
!methodRemoval: FormCanvas class #downBorderForm!
FormCanvas class removeSelector: #downBorderForm!
!methodRemoval: FormCanvas class #downInsideForm!
FormCanvas class removeSelector: #downInsideForm!
!methodRemoval: FormCanvas class #upBorderForm!
FormCanvas class removeSelector: #upBorderForm!
!methodRemoval: FormCanvas class #upInsideForm!
FormCanvas class removeSelector: #upInsideForm!
!methodRemoval: CursorWithAlpha class #buildBiggerNormal!
CursorWithAlpha class removeSelector: #buildBiggerNormal!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	CursorWithAlpha biggerNormal show.
	(FormCanvas arrowWithGradientOfDirection: #up) display.
	(FormCanvas arrowWithGradientOfDirection: #down) display.
	(FormCanvas arrowWithGradientOfDirection: #left) display.
	(FormCanvas arrowWithGradientOfDirection: #right) display.!


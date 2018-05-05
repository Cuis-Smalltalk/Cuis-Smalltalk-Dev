'From Cuis 5.0 of 7 November 2016 [latest update: #3296] on 30 April 2018 at 8:28:45 pm'!

!Preferences class methodsFor: 'menu icons' stamp: 'jmv 4/30/2018 20:13:31'!
useMenuIcons

	self setPreference: #wantsMenuIcons toValue: true! !

!Preferences class methodsFor: 'menu icons' stamp: 'jmv 4/30/2018 20:13:34'!
useNoMenuIcons

	self setPreference: #wantsMenuIcons toValue: false! !

!Preferences class methodsFor: 'menu icons' stamp: 'jmv 4/30/2018 20:13:53'!
wantsMenuIcons
	^ self
		valueOfFlag: #wantsMenuIcons
		ifAbsent: [ true ]! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 4/30/2018 20:26:39'!
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

	StrikeFont removeMostFonts.
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


!Preferences class methodsFor: 'themes' stamp: 'jmv 4/30/2018 20:22:36'!
cuisDefaults
	"
	Preferences cuisDefaults
	"
	self setPreferencesFrom:

	#(
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds false)
		(checkForSlips true)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl true)
		(optionalButtons true)
		(extraDebuggerButtons true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe false)
		(syntaxHighlightingAsYouType true)
		(tapAndHoldEmulatesButton2 true)
		(clickGrabsMorphs false)

		(syntaxHighlightingAsYouTypeAnsiAssignment false)
		(syntaxHighlightingAsYouTypeLeftArrowAssignment false)
	).
	self useMenuIcons
	".
	Theme beCurrent.
	Taskbar showTaskbar
	"! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 4/30/2018 20:24:54'!
slowMachine
	"
	Preferences slowMachine
	"
	self setPreferencesFrom: #(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
	).
	self useNoMenuIcons.
	self runningWorld backgroundImageData: nil.
	"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hideTaskbar
	"! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 4/30/2018 20:24:56'!
smalltalk80
	"A traditional monochrome Smalltalk-80 look and feel, clean and austere, and lacking many features added to Squeak and Cuis in recent years. Caution: turns off the 'smartUpdating' feature that keeps multiple browsers in synch, and much more.

	Preferences smalltalk80
	"

	self setPreferencesFrom:

	#(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(subPixelRenderFonts true)
		(thoroughSenders false)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
	).
	self useNoMenuIcons.
	"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hideTaskbar
	"! !


!Theme methodsFor: 'menus' stamp: 'jmv 4/30/2018 20:06:28'!
basicIcons

	"Minimal menu scheme.
	Theme current class beCurrent
	"

	^self iconsDefinitionFor: #addBasicIconsTo:! !

!Theme methodsFor: 'menus' stamp: 'jmv 4/30/2018 20:17:26'!
menuDecorations

	"build a dictionary wordings -> icon to decorate the menus all over the image"

	^Preferences wantsMenuIcons
		ifTrue: [ self allIcons ]
		ifFalse: [#() ]! !

!Theme methodsFor: 'menus' stamp: 'jmv 4/30/2018 20:06:14'!
miscellaneousIcons
	
	^self iconsDefinitionFor: #addMiscellaneousIconsTo:! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 4/30/2018 20:22:02'!
changeIcons

	| menu selector |
	
	menu _ SelectionMenu
		fromArray: #(
			#( 'Use icons for menu entries' #useMenuIcons )
			#( 'Don''t use icons for menu entries' #useNoMenuIcons )
		).
			
	selector _ menu startUpWithCaption: 'Menu Icons'.
	
	selector ifNotNil: [
		Preferences perform: selector.
		Theme current class beCurrent ]
! !

!methodRemoval: Theme #basicIconsTypeSelector!
Theme removeSelector: #basicIconsTypeSelector!
!methodRemoval: Theme #miscellaneousIconsTypeSelector!
Theme removeSelector: #miscellaneousIconsTypeSelector!
!methodRemoval: Theme #noIcons!
Theme removeSelector: #noIcons!
!methodRemoval: Preferences class #menuIcons!
Preferences class removeSelector: #menuIcons!
!methodRemoval: Preferences class #useAllIcons!
Preferences class removeSelector: #useAllIcons!
!methodRemoval: Preferences class #useBasicIcons!
Preferences class removeSelector: #useBasicIcons!
!methodRemoval: Preferences class #useNoIcons!
Preferences class removeSelector: #useNoIcons!
